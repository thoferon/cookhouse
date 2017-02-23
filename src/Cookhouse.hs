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

import Network.Wai.Handler.Warp

import Cookhouse.App
import Cookhouse.Config
import Cookhouse.Environment
import Cookhouse.Options
import Cookhouse.Plugins.Types
import Cookhouse.Workers.JobWorker
import Cookhouse.Workers.TriggerWorker

processOptions :: (Options -> Config -> IO ()) -> IO ()
processOptions cont = do
  opts   <- parseOptions
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
    then void $ forkProcess $ createSession >> cont opts config
    else cont opts config

webServer :: Environment -> IO ()
webServer env = run (configPort (envConfig env)) (app env)

defaultMain :: [AuthenticationPlugin] -> [TriggerPlugin] -> [SourcePlugin]
            -> [StepPlugin] -> IO ()
defaultMain authPlugins sourcePlugins triggerPlugins stepPlugins = do
  processOptions $ \opts config -> do
    pool <- mkConnectionPool config
    let env = mkEnvironment opts config pool authPlugins sourcePlugins
                            triggerPlugins stepPlugins
    void $ forkOS $ triggerWorker env
    void $ forkOS $ jobWorker     env
    webServer env
