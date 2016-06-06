module Cookhouse
  ( readConfigFromArgs
  , mkEnvironment
  , webServer
  , triggerWorker
  ) where

import Data.Maybe

import System.Environment

import Web.Spock.Simple

import Cookhouse.App
import Cookhouse.Config
import Cookhouse.Environment
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
