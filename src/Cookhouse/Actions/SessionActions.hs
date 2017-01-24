module Cookhouse.Actions.SessionActions
  ( module Cookhouse.Actions.SessionActions
  , Token
  ) where

import Control.Monad.Except

import Cookhouse.Actions.Types
import Cookhouse.Environment
import Cookhouse.Errors
import Cookhouse.Plugins.Types

data Credentials = Credentials
  { plugin   :: String
  , username :: String
  , password :: String
  } deriving Generic

instance FromForm Credentials

signinAction :: Credentials -> Action Token
signinAction Credentials{..} = do
  authPlugin <- getAuthenticationPlugin plugin
  eRes <- runPlugin $ authPluginSignin authPlugin username password []
  case eRes of
    Left  err      -> throwError $ AuthenticationPluginError plugin err
    Right Nothing  -> throwError InvalidCredentials
    Right (Just t) -> return t

data AuthInfo = AuthInfo
  { plugin :: String
  , token  :: Token
  } deriving Generic

instance FromForm AuthInfo

signoutAction :: AuthInfo -> Action NoContent
signoutAction AuthInfo{..} = do
  authPlugin <- getAuthenticationPlugin plugin
  eRes <- runPlugin $ authPluginSignout authPlugin token []
  case eRes of
    Left  err -> throwError $ AuthenticationPluginError plugin err
    Right ()  -> return NoContent
