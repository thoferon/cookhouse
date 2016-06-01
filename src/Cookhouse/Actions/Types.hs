module Cookhouse.Actions.Types where

import Data.List

import Database.PostgreSQL.Simple

import Web.Spock.Simple

import Cookhouse.Config
import Cookhouse.Data.Project
import Cookhouse.Environment
import Cookhouse.Plugins.Types

type AppSpockM a = SpockM Connection () Environment a

type AppSpockAction a = SpockAction Connection () Environment a

getEnvironment :: AppSpockAction Environment
getEnvironment = getState

getAuthenticationPlugins :: AppSpockAction [AuthenticationPlugin]
getAuthenticationPlugins = envAuthenticationPlugins <$> getEnvironment

getAuthenticationPlugin :: String -> AppSpockAction (Maybe AuthenticationPlugin)
getAuthenticationPlugin name = do
  plugins <- getAuthenticationPlugins
  return $ find ((==name) . authPluginName) plugins

getConfig :: AppSpockAction Config
getConfig = envConfig <$> getEnvironment

getProjects :: AppSpockAction [Project]
getProjects = configProjects <$> getConfig
