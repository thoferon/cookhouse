module Cookhouse
  ( readConfigFromArgs
  , mkEnvironment
  , webServer
  , triggerWorker
  , jobWorker
  , defaultMain
  ) where

import Control.Concurrent
import Control.Monad

import Data.Maybe

import System.Environment

import Web.Spock.Simple

import Cookhouse.App
import Cookhouse.Config
import Cookhouse.Environment
import Cookhouse.Plugins.Types
import Cookhouse.Workers.JobWorker
import Cookhouse.Workers.TriggerWorker

readConfigFromArgs :: IO Config
readConfigFromArgs = do
  args <- getArgs
  readConfigOrDie $ maybe "config.yml" id $ listToMaybe args

webServer :: Environment -> IO ()
webServer env = do
  let config = envConfig env
  pool <- mkConnectionPool config
  let spockConfig = defaultSpockCfg () (PCPool pool) env
  runSpock (configPort config) $ spock spockConfig (app config)

defaultMain :: [AuthenticationPlugin] -> [TriggerPlugin] -> [SourcePlugin]
            -> [StepPlugin] -> IO ()
defaultMain authPlugins sourcePlugins triggerPlugins stepPlugins = do
  config <- readConfigFromArgs
  let env = mkEnvironment config authPlugins sourcePlugins
                          triggerPlugins stepPlugins
  void $ forkOS $ triggerWorker env
  void $ forkOS $ jobWorker     env
  webServer env
