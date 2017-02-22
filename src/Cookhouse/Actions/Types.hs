{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cookhouse.Actions.Types
  ( module Cookhouse.Actions.Types
  , module Servant.API
  , module GHC.Generics
  , module Web.FormUrlEncoded
  , module Data.Aeson
  ) where

import GHC.Generics

import Control.Monad.Except
import Control.Monad.Reader

import Data.Aeson
import Data.Time

import Database.Seakale.PostgreSQL

import Web.FormUrlEncoded

import Servant
import Servant.API

import Cookhouse.Capabilities
import Cookhouse.Data.Types
import Cookhouse.Environment
import Cookhouse.Errors
import Cookhouse.Plugins.Types

type SubAction =
  ExceptT CookhouseError (TimeT (StoreT (ReaderT Environment IO)))
type Action = SafeAccessT CookhouseAccess SubAction

runSubAction :: Environment -> SubAction a -> IO (Either CookhouseError a)
runSubAction env action = do
  now  <- liftIO $ getCurrentTime
  eRes <- flip runReaderT env $
    runRequestT defaultPSQL $ runStoreT $ runTimeT now $ runExceptT action
  return $ case eRes of
    Left  err -> Left $ SQLError err
    Right res -> res

actionToSubAction :: Maybe Token -> Maybe String -> Action a -> SubAction a
actionToSubAction mToken mName action = do
  cap <- case (mToken, mName) of
    (Just token, Just name) -> do
      plugin <- getAuthenticationPlugin name
      eRes <- runPlugin $ do
        level <- authPluginGetAccessLevel plugin token []
        return $ toCookhouseCapability level
      either (throwError . AuthenticationPluginError name) return eRes
    _ -> return anonymousCapability
  eRes <- runSafeAccessT action [cap]
  either (throwError . PermissionError) return eRes

subActionToHandler :: Environment -> SubAction a -> Handler a
subActionToHandler env action = do
  eRes <- liftIO $ runSubAction env action
  either (throwError . toServantError) return eRes
