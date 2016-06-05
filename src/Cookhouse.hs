module Cookhouse
  ( readConfigFromArgs
  , webServer
  ) where

import Data.Maybe

import System.Environment

import Data.Pool

import Web.Spock.Simple

import Cookhouse.App
import Cookhouse.Config
import Cookhouse.Environment
import Cookhouse.Plugins.Types

readConfigFromArgs :: IO Config
readConfigFromArgs = do
  args <- getArgs
  readConfigOrDie $ maybe "config.yml" id $ listToMaybe args

webServer :: Config -> [AuthenticationPlugin] -> [TriggerPlugin]
          -> [SourcePlugin] -> IO ()
webServer config authPlugins triggerPlugins sourcePlugins = do
  pool <- createPool (createConn config) destroyConn 3 (24*60*60) 1
  let env = (defaultEnvironment config)
        { envAuthenticationPlugins = authPlugins
        , envTriggerPlugins        = triggerPlugins
        , envSourcePlugins         = sourcePlugins
        }
      cfg = defaultSpockCfg () (PCPool pool) env
  runSpock (configPort config) $ spock cfg (app config)
