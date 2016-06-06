{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module SpecHelpers
  ( module SpecHelpers
  , module Debug.Trace
  , module Data.Either
  , module Data.Maybe
  , module Test.Hspec
  , module Test.Hspec.QuickCheck
  , module Test.QuickCheck
  , module Database.PostgreSQL.Simple
  , module Database.PostgreSQL.Simple.ToField
  , module Database.PostgreSQL.Simple.ToRow
  , module Cookhouse.Capabilities
  , module Cookhouse.Data.Types
  , module Cookhouse.Errors
  ) where

import           Debug.Trace

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Free
import           Control.Monad.State
import           Control.Monad.Trans.Free
import           Control.SafeAccess

import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import qualified Data.ByteString.Char8 as BS

import           Database.PostgreSQL.Simple hiding (Fixed)
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

import           Cookhouse.Capabilities
import           Cookhouse.Data.Types
import           Cookhouse.Errors

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

type EmulatorM = StateT DatabaseEmulator (Either String)

failEmulator :: String -> EmulatorM a
failEmulator = lift . Left

changeEmulator :: (DatabaseEmulator -> DatabaseEmulator) -> EmulatorM ()
changeEmulator f = state $ \emulator -> ((), f emulator)

data DatabaseEmulator = DatabaseEmulator
  { deInsert :: forall p. ToRow p => Query -> [Query] -> Query -> [p]
             -> EmulatorM [PureRow]
  , deSelect :: forall a. Query -> [Query] -> SelectModifiers a
             -> EmulatorM [PureRow]
  , deUpdate :: forall a. Query -> [(Query, Action)] -> SelectModifiers a
             -> EmulatorM Int64
  , deDelete :: forall a. Query -> SelectModifiers a -> EmulatorM Int64
  }

instance Monoid DatabaseEmulator where
  mempty = DatabaseEmulator
    { deInsert = \_ _ _ _ ->
        failEmulator "no support for INSERT in the emulator"
    , deSelect = \_ _ _   ->
        failEmulator "no support for SELECT in the emulator"
    , deUpdate = \_ _ _   ->
        failEmulator "no support for UPDATE in the emulator"
    , deDelete = \_ _     ->
        failEmulator "no support for DELETE in the emulator"
    }
  mappend e1 e2 = DatabaseEmulator
    { deInsert = \tbl columns ret params -> do
        deInsert e1 tbl columns ret params
        <|> deInsert e2 tbl columns ret params
    , deSelect = \tbl columns mods ->
        deSelect e1 tbl columns mods <|> deSelect e2 tbl columns mods
    , deUpdate = \tbl pairs mods ->
        deUpdate e1 tbl pairs mods <|> deUpdate e2 tbl pairs mods
    , deDelete = \tbl mods -> deDelete e1 tbl mods <|> deDelete e2 tbl mods
    }

data TestError
  = EmulatorError String
  | DataLayerError CookhouseError
  | AccessError CookhouseAccess
  deriving (Show, Eq)

someTime :: UTCTime
someTime = UTCTime { utctDay = ModifiedJulianDay 58000, utctDayTime = 40000 }

testWithTime :: UTCTime -> Capability CookhouseAccess -> DatabaseEmulator
             -> DataM a -> Either TestError a
testWithTime time cap emulator action =
   let eRes = fmap fst $ flip runStateT emulator $
          foldFree databaseInterpreter $ iterT timeInterpreter $
            runExceptT $ flip runSafeAccessT [cap] $ action

    in case eRes of
         Left  err                 -> Left $ EmulatorError err
         Right (Left  err)         -> Left $ DataLayerError err
         Right (Right (Left  err)) -> Left $ AccessError err
         Right (Right (Right res)) -> Right res

  where
    timeInterpreter :: TimeF (m a) -> m a
    timeInterpreter (GetTime f) = f time

    databaseInterpreter :: DatabaseF a -> EmulatorM a
    databaseInterpreter d = do
      (DatabaseEmulator{..}) <- Control.Monad.State.get
      case d of
        DBInsert tbl columns ret params f ->
          f <$> deInsert tbl columns ret params
        DBSelect tbl columns mods f -> f <$> deSelect tbl columns mods
        DBUpdate tbl pairs   mods f -> f <$> deUpdate tbl pairs   mods
        DBDelete tbl         mods f -> f <$> deDelete tbl         mods

test :: Capability CookhouseAccess -> DatabaseEmulator -> DataM a
     -> Either TestError a
test = testWithTime someTime

varchar :: BS.ByteString -> PureField
varchar = PureField "varchar" . Just

integer :: Integer -> PureField
integer = PureField "int4" . Just . BS.pack . show

timestamp :: BS.ByteString -> PureField
timestamp = PureField "timestamp" . Just

timestamp' :: UTCTime -> PureField
timestamp' = timestamp . BS.pack . formatTime defaultTimeLocale "%F %T%Q"

boolean :: Bool -> PureField
boolean True  = PureField "boolean" $ Just "t"
boolean False = PureField "boolean" $ Just "f"

nullValue :: BS.ByteString -> PureField
nullValue = flip PureField Nothing

emptyArray :: BS.ByteString -> PureField
emptyArray = flip array "{}"

array :: BS.ByteString -> BS.ByteString -> PureField
array name value = PureField ("_" <> name) (Just value)

isIncludedIn :: Eq a => [a] -> [a] -> Bool
isIncludedIn xs ys = all (`elem` ys) xs
