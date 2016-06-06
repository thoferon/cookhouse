{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Cookhouse.Data.Types
  ( module Cookhouse.Data.Types
  , module Cookhouse.Data.Internal
  , throwError
  , Int64
  ) where

import           GHC.Int

import           Control.Monad.Except
import           Control.Monad.Identity

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text             as T

import           Database.PostgreSQL.Simple.Types

import           Cookhouse.Capabilities
import           Cookhouse.Data.Internal
import           Cookhouse.Errors

type DataM
  = SafeAccessT CookhouseAccess (ExceptT CookhouseError (TimeT DatabaseM))

instance (Storable a, ToJSON a, ToJSON (EntityID a)) => ToJSON (Entity a) where
  toJSON = runIdentity . entityToJSONWith (Identity . toJSON)

entityToJSONWith :: (Monad m, Storable a, ToJSON (EntityID a)) => (a -> m Value)
                 -> Entity a -> m Value
entityToJSONWith f (Entity i v) = do
  let idKey = T.pack . BS.unpack . fromQuery . idFieldName . mkProxy $ v
  jval <- f v
  return $ object [idKey .= i, "data" .= jval]
