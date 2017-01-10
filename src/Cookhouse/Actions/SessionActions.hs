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

instance FromJSON Credentials

signinAction :: Credentials -> Action Token
signinAction Credentials{..} = do
  authPlugin <- getAuthenticationPlugin plugin
  eRes <- runPlugin $ authPluginSignin authPlugin username password
  case eRes of
    Left  err      -> throwError $ AuthenticationPluginError plugin err
    Right Nothing  -> throwError InvalidCredentials
    Right (Just t) -> return t

data AuthInfo = AuthInfo
  { aiPlugin :: String
  , aiToken  :: Token
  }

instance FromJSON AuthInfo where
  parseJSON = withObject "AuthInfo" $ \obj ->
    AuthInfo <$> obj .: "plugin" <*> obj .: "token"

signoutAction :: AuthInfo -> Action NoContent
signoutAction AuthInfo{..} = do
  authPlugin <- getAuthenticationPlugin aiPlugin
  eRes <- runPlugin $ authPluginSignout authPlugin aiToken
  case eRes of
    Left  err -> throwError $ AuthenticationPluginError aiPlugin err
    Right ()  -> return NoContent
