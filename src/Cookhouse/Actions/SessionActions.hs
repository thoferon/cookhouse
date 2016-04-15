module Cookhouse.Actions.SessionActions where

import Control.Monad.Except

import Cookhouse.Actions.Helpers
import Cookhouse.Errors
import Cookhouse.Plugins.Types

signinAction :: AppSpockAction ()
signinAction = do
  let pof = (,,)
        <$> paramPOF "plugin"
        <*> paramPOF "username"
        <*> paramPOF "password"

  runPOF pof $ \(name, username, password) -> do
    mPlugin <- getAuthenticationPlugin name
    case mPlugin of
      Nothing -> failAction $ isInvalidError "plugin"
      Just plugin -> do
        eRes <- liftIO $ runExceptT $ authPluginSignin plugin username password
        case eRes of
          Left err -> failAction $ AuthenticationPluginError name err
          Right token -> do
            setStatus ok200
            json $ object [ "token" .= token ]

signoutAction :: AppSpockAction ()
signoutAction = do
  mToken  <- getToken
  mName   <- getAuthenticationPluginName
  mPlugin <- maybe (return Nothing) getAuthenticationPlugin mName

  case (mToken, mPlugin) of
    (Just token, Just plugin) -> do
      void $ liftIO $ runExceptT $ authPluginSignout plugin token
      setStatus noContent204
    _ -> setStatus noContent204
