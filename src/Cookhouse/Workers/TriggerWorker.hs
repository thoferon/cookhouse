module Cookhouse.Workers.TriggerWorker
  ( triggerWorker
  ) where

import Control.Concurrent
import Control.Monad.Except

import Data.Time

import System.Exit

import Cookhouse.Capabilities
import Cookhouse.Config
import Cookhouse.Data.Project
import Cookhouse.Environment
import Cookhouse.Errors
import Cookhouse.Logic.JobGeneration
import Cookhouse.Plugins.Types
import Cookhouse.Workers.Helpers

triggerWorker :: Environment -> IO ()
triggerWorker env = do
  pool <- mkConnectionPool $ envConfig env
  eRes <- runWorker env triggerWorkerCapability pool $ do
    forEveryMinute $ \hour minute ->
      catchError (checkTriggers hour minute) logError
  case eRes of
    Left  err -> logError err
    Right ()  -> return ()
  exitFailure -- not supposed to terminate

checkTriggers :: Int -> Int -> WorkerM ()
checkTriggers hour minute = do
  projects <- getProjects
  let pairs =
        [ (project, trigger)
        | project <- projects
        , trigger <- projectTriggers project
        , doesTimeSpecMatch (triggerTimeSpec trigger) hour minute
        ]

  forM_ pairs $ \(project, Trigger{..}) -> flip catchError logError $ do
    let Source{..} = projectSource project
    tplugin <- getTriggerPlugin triggerPlugin
    splugin <- getSourcePlugin  sourcePlugin

    projectDir <- getProjectDirectory project
    let fetchRepo path =
          sourcePluginFetch splugin sourceLocation path sourceConfig
        checkRepo path =
          sourcePluginPull splugin sourceLocation path sourceConfig

    eRes <- runPlugin $
      triggerPluginCheck tplugin fetchRepo checkRepo projectDir triggerConfig

    case eRes of
      Left  err   -> throwError $ TriggerPluginError triggerPlugin err
      Right False -> return ()
      Right True  -> inDataLayer $ do
        void $ generateJobs projects $ projectIdentifier project

-- | Run the continuation every minute
forEveryMinute :: MonadIO m => (Int -> Int -> m ()) -> m ()
forEveryMinute cont = do
    now <- liftIO $ zonedTimeToLocalTime <$> getZonedTime
    go cont now

  where
    next :: (Int, Int) -> (Int, Int)
    next (23,59) = (0,0)
    next (h,59)  = (h+1,0)
    next (h,m)   = (h,m+1)

    go :: MonadIO m => (Int -> Int -> m ()) -> LocalTime -> m ()
    go _cont previous = do
      now <- liftIO $ zonedTimeToLocalTime <$> getZonedTime
      let previousTime = localTimeOfDay previous
          previousHour = todHour previousTime
          previousMin  = todMin  previousTime
          currentTime  = localTimeOfDay now
          currentHour  = todHour currentTime
          currentMin   = todMin  currentTime

      if previousHour == currentHour && previousMin == currentMin
        then do
          let sec = round $ todSec currentTime
          liftIO $ threadDelay $ (60 - sec) * 10 ^ (6 :: Int)
          go _cont previous
        else do
          let notDone = drop 1 $ iterate next (previousHour, previousMin)
              toDo    = takeWhile ((/= next (currentHour, currentMin))) notDone
          mapM_ (uncurry _cont) toDo
          go _cont now
