module Cookhouse.Environment
  ( Environment(..)
  , defaultEnvironment
  , mkEnvironment
  , HasEnvironment(..)
  , getAuthenticationPlugins
  , findAuthenticationPlugin
  , getAuthenticationPlugin
  , getTriggerPlugins
  , findTriggerPlugin
  , getTriggerPlugin
  , getSourcePlugins
  , findSourcePlugin
  , getSourcePlugin
  , getConfig
  , getProjects
  , getProjectDirectory
  ) where

import Control.Monad.Except

import Data.List

import System.Directory
import System.FilePath

import Cookhouse.Config
import Cookhouse.Data.Project
import Cookhouse.Errors
import Cookhouse.Plugins.Types

data Environment = Environment
  { envAuthenticationPlugins :: [AuthenticationPlugin]
  , envTriggerPlugins        :: [TriggerPlugin]
  , envSourcePlugins         :: [SourcePlugin]
  , envConfig                :: Config
  }

defaultEnvironment :: Config -> Environment
defaultEnvironment config = Environment
  { envAuthenticationPlugins = []
  , envTriggerPlugins        = []
  , envSourcePlugins         = []
  , envConfig                = config
  }

mkEnvironment :: Config -> [AuthenticationPlugin] -> [TriggerPlugin]
              -> [SourcePlugin] -> Environment
mkEnvironment config authPlugins triggerPlugins sourcePlugins =
  (defaultEnvironment config)
    { envAuthenticationPlugins = authPlugins
    , envTriggerPlugins        = triggerPlugins
    , envSourcePlugins         = sourcePlugins
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
