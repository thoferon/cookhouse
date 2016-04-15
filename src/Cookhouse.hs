module Cookhouse
  ( defaultMain
  ) where

import Data.Maybe

import System.Environment

import Data.Pool

import Web.Spock.Simple

import Cookhouse.App
import Cookhouse.Config
import Cookhouse.Environment
import Cookhouse.Plugins.Types

defaultMain :: [AuthenticationPlugin] -> IO ()
defaultMain authPlugins = do
  args   <- getArgs
  config <- readConfigOrDie $ maybe "config.yml" id $ listToMaybe args

  pool <- createPool (createConn config) destroyConn 3 (24*60*60) 1
  let env = (defaultEnvironment config)
        { envAuthenticationPlugins = authPlugins }
      cfg = defaultSpockCfg () (PCPool pool) env
  runSpock (configPort config) $ spock cfg (app config)
