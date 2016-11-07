module Cookhouse.Environment where

import Control.Monad.Except

import Data.List

import System.Directory
import System.FilePath

import Cookhouse.Config
import Cookhouse.Data.Job
import Cookhouse.Data.Project
import Cookhouse.Errors
import Cookhouse.Plugins.Types

data Environment = Environment
  { envAuthenticationPlugins :: [AuthenticationPlugin]
  , envTriggerPlugins        :: [TriggerPlugin]
  , envSourcePlugins         :: [SourcePlugin]
  , envStepPlugins           :: [StepPlugin]
  , envConfig                :: Config
  }

defaultEnvironment :: Config -> Environment
defaultEnvironment config = Environment
  { envAuthenticationPlugins = []
  , envTriggerPlugins        = []
  , envSourcePlugins         = []
  , envStepPlugins           = []
  , envConfig                = config
  }

mkEnvironment :: Config -> [AuthenticationPlugin] -> [TriggerPlugin]
              -> [SourcePlugin] -> [StepPlugin] -> Environment
mkEnvironment config authPlugins triggerPlugins sourcePlugins stepPlugins =
  (defaultEnvironment config)
    { envAuthenticationPlugins = authPlugins
    , envTriggerPlugins        = triggerPlugins
    , envSourcePlugins         = sourcePlugins
    , envStepPlugins           = stepPlugins
    }

class Functor f => HasEnvironment f where
  getEnvironment :: f Environment

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

getConfig :: HasEnvironment f => f Config
getConfig = envConfig <$> getEnvironment

getProjects :: HasEnvironment f => f [Project]
getProjects = configProjects <$> getConfig

getProjectDirectory :: (HasEnvironment m, MonadIO m) => Project -> m FilePath
getProjectDirectory (Project{..}) = do
  Config{..} <- getConfig
  let path = configBuildDirectory </> unProjectIdentifier projectIdentifier
  liftIO $ createDirectoryIfMissing True path
  return path

getJobDirectory :: (HasEnvironment m, MonadIO m) => Project -> Job -> m FilePath
getJobDirectory project job = do
  projectDir <- getProjectDirectory project
  return $ projectDir </> jobDirectory job
