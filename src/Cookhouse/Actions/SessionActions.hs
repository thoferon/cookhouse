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
    mPlugin <- findAuthenticationPlugin name
    case mPlugin of
      Nothing -> failAction $ isInvalidError "plugin"
      Just plugin -> do
        eRes <- runPlugin $ authPluginSignin plugin username password
        case eRes of
          Left err -> failAction $ AuthenticationPluginError name err
          Right token -> do
            setStatus ok200
            json $ object [ "token" .= token ]

signoutAction :: AppSpockAction ()
signoutAction = do
  mToken  <- getToken
  mName   <- getAuthenticationPluginName
  mPlugin <- maybe (return Nothing) findAuthenticationPlugin mName

  case (mToken, mPlugin) of
    (Just token, Just plugin) -> do
      void $ runPlugin $ authPluginSignout plugin token
      setStatus noContent204
    _ -> setStatus badRequest400
