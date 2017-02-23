{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cookhouse.Environment where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control

import Data.List
import Data.Pool

import System.Directory
import System.FilePath

import Database.Seakale.PostgreSQL

import Cookhouse.Config
import Cookhouse.Data.Job
import Cookhouse.Data.Project
import Cookhouse.Errors
import Cookhouse.Options
import Cookhouse.Plugins.Types

data Environment = Environment
  { envAuthenticationPlugins :: [AuthenticationPlugin]
  , envTriggerPlugins        :: [TriggerPlugin]
  , envSourcePlugins         :: [SourcePlugin]
  , envStepPlugins           :: [StepPlugin]
  , envConnectionPool        :: Pool Connection
  , envOptions               :: Options
  , envConfig                :: Config
  }

defaultEnvironment :: Options -> Config -> Pool Connection -> Environment
defaultEnvironment opts config pool = Environment
  { envAuthenticationPlugins = []
  , envTriggerPlugins        = []
  , envSourcePlugins         = []
  , envStepPlugins           = []
  , envConnectionPool        = pool
  , envOptions               = opts
  , envConfig                = config
  }

mkEnvironment :: Options -> Config -> Pool Connection -> [AuthenticationPlugin]
              -> [TriggerPlugin] -> [SourcePlugin] -> [StepPlugin]
              -> Environment
mkEnvironment opts config pool authPlugins triggerPlugins sourcePlugins
              stepPlugins =
    let authPlugins' =
          helper authPluginName authPluginWithDefaultConfig authPlugins
        triggerPlugins' =
          helper triggerPluginName triggerPluginWithDefaultConfig triggerPlugins
        sourcePlugins' =
          helper sourcePluginName sourcePluginWithDefaultConfig sourcePlugins
        stepPlugins' =
          helper stepPluginName stepPluginWithDefaultConfig stepPlugins

    in (defaultEnvironment opts config pool)
         { envAuthenticationPlugins = authPlugins'
         , envTriggerPlugins        = triggerPlugins'
         , envSourcePlugins         = sourcePlugins'
         , envStepPlugins           = stepPlugins'
         }

  where
    helper :: (a -> String) -> (a -> PluginConfig -> a) -> [a] -> [a]
    helper _ _ [] = []
    helper getName setDefaults (pl : pls) =
      case lookup (getName pl) (configPlugins config) of
        Nothing  -> pl                 : helper getName setDefaults pls
        Just cfg -> setDefaults pl cfg : helper getName setDefaults pls

class Functor f => HasEnvironment f where
  getEnvironment :: f Environment

instance Monad m => HasEnvironment (ReaderT Environment m) where
  getEnvironment = ask

instance {-# OVERLAPPABLE #-} ( HasEnvironment m, Monad m, MonadTrans t
                              , Functor (t m), Monad (t m) )
  => HasEnvironment (t m) where
  getEnvironment = lift getEnvironment

getAuthenticationPlugins :: HasEnvironment f => f [AuthenticationPlugin]
getAuthenticationPlugins = envAuthenticationPlugins <$> getEnvironment

findAuthenticationPlugin :: HasEnvironment f => String
                         -> f (Maybe AuthenticationPlugin)
findAuthenticationPlugin name =
  find ((==name) . authPluginName) <$> getAuthenticationPlugins

getAuthenticationPlugin :: (HasEnvironment m, MonadError CookhouseError m)
                        => String -> m AuthenticationPlugin
getAuthenticationPlugin name = do
  mPlugin <- findAuthenticationPlugin name
  maybe (throwError $ MissingPluginError name) return mPlugin

getTriggerPlugins :: HasEnvironment f => f [TriggerPlugin]
getTriggerPlugins = envTriggerPlugins <$> getEnvironment

findTriggerPlugin :: HasEnvironment f => String -> f (Maybe TriggerPlugin)
findTriggerPlugin name =
  find ((==name) . triggerPluginName) <$> getTriggerPlugins

getTriggerPlugin :: (HasEnvironment m, MonadError CookhouseError m)
                 => String -> m TriggerPlugin
getTriggerPlugin name = do
  mPlugin <- findTriggerPlugin name
  maybe (throwError $ MissingPluginError name) return mPlugin

getSourcePlugins :: HasEnvironment f => f [SourcePlugin]
getSourcePlugins = envSourcePlugins <$> getEnvironment

findSourcePlugin :: HasEnvironment f => String -> f (Maybe SourcePlugin)
findSourcePlugin name =
  find ((==name) . sourcePluginName) <$> getSourcePlugins

getSourcePlugin :: (HasEnvironment m, MonadError CookhouseError m)
                 => String -> m SourcePlugin
getSourcePlugin name = do
  mPlugin <- findSourcePlugin name
  maybe (throwError $ MissingPluginError name) return mPlugin

getStepPlugins :: HasEnvironment f => f [StepPlugin]
getStepPlugins = envStepPlugins <$> getEnvironment

findStepPlugin :: HasEnvironment f => String -> f (Maybe StepPlugin)
findStepPlugin name =
  find ((==name) . stepPluginName) <$> getStepPlugins

getStepPlugin :: (HasEnvironment m, MonadError CookhouseError m)
              => String -> m StepPlugin
getStepPlugin name = do
  mPlugin <- findStepPlugin name
  maybe (throwError $ MissingPluginError name) return mPlugin

getConnectionPool :: HasEnvironment f => f (Pool Connection)
getConnectionPool = envConnectionPool <$> getEnvironment

instance (HasEnvironment m, Monad m, MonadBaseControl IO m)
  => HasConnection m where
  withConn f = do
    pool <- getConnectionPool
    withResource pool f

getOptions :: HasEnvironment f => f Options
getOptions = envOptions <$> getEnvironment

getConfig :: HasEnvironment f => f Config
getConfig = envConfig <$> getEnvironment

getProjects :: HasEnvironment f => f [Project]
getProjects = configProjects <$> getConfig

getProjectDirectory :: (HasEnvironment m, MonadIO m) => Project -> m FilePath
getProjectDirectory Project{..} = do
  Config{..} <- getConfig
  let path = configBuildDirectory </> unProjectIdentifier projectIdentifier
  liftIO $ createDirectoryIfMissing True path
  return path

getJobDirectory :: (HasEnvironment m, MonadIO m) => Project -> Job -> m FilePath
getJobDirectory project job = do
  projectDir <- getProjectDirectory project
  return $ projectDir </> jobDirectory job
