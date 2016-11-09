module Cookhouse
  ( processOptions
  , mkEnvironment
  , webServer
  , triggerWorker
  , jobWorker
  , defaultMain
  ) where

import Control.Concurrent
import Control.Monad

import System.Posix.Process
import System.Posix.User

import Web.Spock.Simple

import Cookhouse.App
import Cookhouse.Config
import Cookhouse.Environment
import Cookhouse.Options
import Cookhouse.Plugins.Types
import Cookhouse.Workers.JobWorker
import Cookhouse.Workers.TriggerWorker

processOptions :: (Config -> IO ()) -> IO ()
processOptions cont = do
  opts   <- getOptions
  config <- readConfigOrDie $ optConfigFile opts

  case optGroup opts of
    Nothing -> return ()
    Just name -> do
      groupEntry <- getGroupEntryForName name
      setGroupID $ groupID groupEntry

  case optUser opts of
    Nothing -> return ()
    Just name -> do
      userEntry <- getUserEntryForName name
      setUserID $ userID userEntry

  if optDaemon opts
    then void $ forkProcess $ createSession >> cont config
    else cont config

webServer :: Environment -> IO ()
webServer env = do
  let config = envConfig env
  pool <- mkConnectionPool config
  let spockConfig = defaultSpockCfg () (PCPool pool) env
  runSpock (configPort config) $ spock spockConfig (app config)

defaultMain :: [AuthenticationPlugin] -> [TriggerPlugin] -> [SourcePlugin]
            -> [StepPlugin] -> IO ()
defaultMain authPlugins sourcePlugins triggerPlugins stepPlugins = do
  processOptions $ \config -> do
    let env = mkEnvironment config authPlugins sourcePlugins
                            triggerPlugins stepPlugins
    void $ forkOS $ triggerWorker env
    void $ forkOS $ jobWorker     env
    webServer env
