module Cookhouse.Plugins.Types
  ( PluginM
  , Token(..)
  , AccessLevel(..)
  , AuthenticationPlugin(..)
  ) where

import           Control.Monad.Except

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS

type PluginM = ExceptT String IO

newtype Token = Token { unToken :: BS.ByteString }

instance ToJSON Token where
  toJSON = toJSON . BS.unpack . unToken

data AccessLevel = Admin | User | ReadOnly

data AuthenticationPlugin = AuthenticationPlugin
  { authPluginName           :: String
  , authPluginSignin         :: String -> String -> PluginM (Maybe Token)
  , authPluginGetAccessLevel :: Token -> PluginM AccessLevel
  , authPluginSignout        :: Token -> PluginM ()
  }
