module Cookhouse.Plugins.Types
  ( PluginM
  , PluginConfig
  , SimpleValue(..)
  -- * Authentication plugins
  , Token(..)
  , AccessLevel(..)
  , AuthenticationPlugin(..)
  -- * Source plugins
  , SourcePlugin(..)
  -- * Trigger plugins
  , TriggerPlugin(..)
  -- * Step plugins
  ) where

import           Control.Applicative
import           Control.Monad.Except

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS

{-
 - Generic types
 -}

type PluginM = ExceptT String IO

type PluginConfig = [(String, SimpleValue)]

data SimpleValue
  = SVBool   Bool
  | SVInt    Int
  | SVString String
  deriving (Eq, Show)

instance FromJSON SimpleValue where
  parseJSON v = do
    (SVBool <$> parseJSON v)
    <|> (SVInt <$> parseJSON v)
    <|> (SVString <$> parseJSON v)

{-
 - Authentication plugins
 -}

newtype Token = Token { unToken :: BS.ByteString }

instance ToJSON Token where
  toJSON = toJSON . BS.unpack . unToken

data AccessLevel = Admin | User | ReadOnly

data AuthenticationPlugin = AuthenticationPlugin
  { authPluginName           :: String
  , authPluginTitle          :: String -- ^ Human-readable name of the plugin
  , authPluginSignin         :: String -- ^ Username
                             -> String -- ^ Password
                             -> PluginM (Maybe Token)
  , authPluginGetAccessLevel :: Token -> PluginM AccessLevel
  , authPluginSignout        :: Token -> PluginM ()
  }

{-
 - Source plugins
 -}

data SourcePlugin = SourcePlugin
  { sourcePluginName  :: String
  , sourcePluginFetch :: String       -- ^ Field 'location' in the config
                      -> FilePath     -- ^ Local directory to pull the repo to
                      -> PluginConfig -- ^ Extra configuration
                      -> PluginM ()
  , sourcePluginPull  :: String       -- ^ Field 'location' in the config
                      -> FilePath     -- ^ Local directory to pull the repo to
                      -> PluginConfig -- ^ Extra configuration
                      -> PluginM Bool -- ^ Whether there was something to pull
  }

{-
 - Trigger plugins
 -}

data TriggerPlugin = TriggerPlugin
  { triggerPluginName  :: String
  , triggerPluginCheck :: (FilePath -> PluginM ())
                          -- ^ A function to make the source plugin fetch the
                          -- repository
                       -> (FilePath -> PluginM Bool)
                          -- ^ A function to make the source plugin check for
                          -- changes
                       -> FilePath     -- ^ Directory of the project
                       -> PluginConfig -- ^ Extra configuration
                       -> PluginM Bool
  }

{-
 - Step plugins
 -}
