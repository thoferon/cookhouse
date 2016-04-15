{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Cookhouse.Data.Types
  ( module Cookhouse.Data.Types
  , throwError
  ) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader

import           Data.Aeson
import           Data.Time
import           Data.Typeable hiding (Proxy(..))
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text                  as T

import           Web.PathPieces

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField hiding (name)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.Types

import           Cookhouse.Capabilities
import           Cookhouse.Errors

data Proxy a = Proxy

mkProxy :: a -> Proxy a
mkProxy = const Proxy

type MonadDataLayer m = ( MonadIO m
                        , MonadSafeAccess CookhouseAccess m
                        , MonadError CookhouseError m
                        , MonadReader Connection m
                        , Functor m
                        )

data Entity a = forall. Storable a => Entity
  { entityID  :: EntityID a
  , entityVal :: a
  }

instance (ToJSON a, ToJSON (EntityID a)) => ToJSON (Entity a) where
  toJSON = runIdentity . entityToJSONWith (Identity . toJSON)

entityToJSONWith :: (Monad m, ToJSON (EntityID a)) => (a -> m Value) -> Entity a
                 -> m Value
entityToJSONWith f (Entity i v) = do
  let idKey = T.pack . BS.unpack . fromQuery . idFieldName . mkProxy $ v
  jval <- f v
  return $ object [idKey .= i, "data" .= jval]

mapEntity :: Storable a => (a -> a) -> Entity a -> Entity a
mapEntity f entity = Entity
  { entityID  = entityID entity
  , entityVal = f $ entityVal entity
  }

instance Storable a => FromRow (Entity a) where
  fromRow = Entity <$> field <*> fromRow

class (FromRow a, ToRow a, FromField (EntityID a), ToField (EntityID a))
    => Storable a where

  data EntityID a :: *

  tableName  :: Proxy a -> Query
  fieldNames :: Proxy a -> [Query]

  idFieldName :: Proxy a -> Query
  idFieldName _ = "id"

-- Provider

data Provider = Oddsportal | Betfair | Pinnacle deriving Typeable

instance FromField Provider where
  fromField f mBS = do
    name <- typename f
    case name of
      "provider" -> case mBS of
        Nothing  -> returnError UnexpectedNull f ""
        Just val -> case stringToProvider $ BS.unpack val of
          Just provider -> return provider
          Nothing -> returnError ConversionFailed f $
            "invalid provider: " ++ BS.unpack val
      _ -> returnError Incompatible f ""

instance ToField Provider where
  toField = toField . providerToString

instance ToJSON Provider where
  toJSON = toJSON . providerToString

instance PathPiece Provider where
  toPathPiece   = toPathPiece . providerToString
  fromPathPiece = (stringToProvider =<<) . fromPathPiece

providerToString :: Provider -> String
providerToString provider = case provider of
  Oddsportal -> "oddsportal"
  Betfair    -> "betfair"
  Pinnacle   -> "pinnacle"

stringToProvider :: String -> Maybe Provider
stringToProvider str = case str of
  "oddsportal" -> Just Oddsportal
  "betfair"    -> Just Betfair
  "pinnacle"   -> Just Pinnacle
  _            -> Nothing

-- TimeAsTimestamp for values of type UTCTime stored as a timestamp

newtype TimeAsTimestamp
  = TimeAsTimestamp { timeFromTimestamp :: UTCTime }
  deriving (Typeable)

instance FromField TimeAsTimestamp where
  fromField f mBS = do
    name <- typename f
    case name of
      "timestamp" -> case mBS of
        Nothing  -> returnError UnexpectedNull f ""
        Just val -> do
          let str = BS.unpack val
              fmt = "%F %T%Q"
          case parseTimeM True defaultTimeLocale fmt str of
            Nothing -> returnError ConversionFailed f ""
            Just t  -> return $ TimeAsTimestamp t
      _ -> returnError Incompatible f ""

instance ToField TimeAsTimestamp where
  toField = toField . formatTime defaultTimeLocale "%F %T" . timeFromTimestamp

newtype JsonAsVarchar
  = JsonAsVarchar { jsonFromVarchar :: Value }
  deriving (Typeable)

instance ToField JsonAsVarchar where
  toField = toField . encode . jsonFromVarchar

instance FromField JsonAsVarchar where
  fromField f mBS = do
    name <- typename f
    case name of
      "varchar" -> case mBS of
        Nothing -> returnError UnexpectedNull f ""
        Just bs ->
          case eitherDecode $ BSL.fromChunks [bs] of
            Left  err -> returnError ConversionFailed f err
            Right val -> return $ JsonAsVarchar val
      _ -> returnError Incompatible f ""

newtype SQLArray a = SQLArray { fromSQLArray :: [a] }

instance ToField a => ToField (SQLArray a) where
  toField (SQLArray xs)
    | null xs   = Escape "{}"
    | otherwise = toField $ PGArray xs

instance (FromField a, Typeable a) => FromField (SQLArray a) where
  fromField f mBS = (SQLArray . fromPGArray) <$> fromField f mBS
