{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cookhouse.Workers.JobWorker
  ( jobWorker
  ) where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.State

import Data.Maybe

import System.Directory
import System.FilePath
import System.IO

import Cookhouse.Capabilities
import Cookhouse.Config
import Cookhouse.Data.Job
import Cookhouse.Data.JobResult
import Cookhouse.Data.Project
import Cookhouse.Data.Types hiding (get)
import Cookhouse.Environment
import Cookhouse.Errors
import Cookhouse.Logic.JobQueue
import Cookhouse.Logic.JobResultOutput
import Cookhouse.Plugins.Types
import Cookhouse.Workers.Helpers

{-
 - Worker management
 -}

type JobWorkerM = StateT [(ThreadId, MVar ())] WorkerM

instance HasEnvironment JobWorkerM where
  getEnvironment = lift getEnvironment

jobWorker :: Environment -> IO ()
jobWorker = workerMain jobWorkerCapability $
  fst <$> runStateT manageJobs []

manageJobs :: JobWorkerM ()
manageJobs = do
  lift $ inDataLayer $ cleanupRunningJobs >> abortUnneededJobs
  maxJobCount <- configMaxJobCount <$> getConfig
  forever $ do
    checkRunningJobs
    threads <- get
    when (length threads < maxJobCount) spawnNextJob
    liftIO $ threadDelay $ 5 * 1000 * 1000 -- 5 seconds

-- This function sets all jobs marked as in-progress to in-queue when we restart
-- the process. The worker in charge has probably been killed.
-- FIXME: Maybe it should store the process ID in the DB and check whether it's
-- still running.
cleanupRunningJobs :: MonadDataLayer m s => m ()
cleanupRunningJobs = do
  ents <- findJobs (JobStatus ==. JobInProgress) mempty
  mapM_ (flip editJob JobInQueue . entityID) ents

checkRunningJobs :: JobWorkerM ()
checkRunningJobs = do
  threads <- get
  forM_ threads $ \(i, mvar) -> do
    finished <- liftIO $ isJust <$> tryReadMVar mvar
    when finished $ removeThread i

spawnNextJob :: JobWorkerM ()
spawnNextJob = do
  mJob <- lift $ inDataLayer getNextJob
  case mJob of
    Nothing  -> return ()
    Just job -> do
      mvar <- liftIO newEmptyMVar
      env  <- getEnvironment

      i <- liftIO $ forkOS $ do
        eRes <- runWorker env jobWorkerCapability $ case job of
          MetaJob Run      ent -> runJob      ent
          MetaJob Rollback ent -> rollbackJob ent
        case eRes of
          Left err -> hPutStrLn stderr $ "Job failed: " ++ show err
          Right () -> return ()
        putMVar mvar ()
      addThread i mvar

addThread :: ThreadId -> MVar () -> JobWorkerM ()
addThread i mvar = state $ \pairs -> ((), (i, mvar) : pairs)

removeThread :: ThreadId -> JobWorkerM ()
removeThread i = state $ \pairs -> ((), filter ((/= i) . fst) pairs)

{-
 - Proper job worker
 -}

runJob :: Entity Job -> WorkerM ()
runJob ent@(Entity jobID _) = do
  inDataLayer $ editJob jobID JobInProgress
  eRes <- flip catchError (return . Left) $ Right <$> performJob Run ent
  inDataLayer $ do
    editJob jobID $ either (const JobFailure) (const JobSuccess) eRes
    abortUnneededJobs

rollbackJob :: Entity Job -> WorkerM ()
rollbackJob ent@(Entity jobID _) = do
  inDataLayer $ editJob jobID JobRollbacked
  performJob Rollback ent
  inDataLayer abortUnneededJobs

performJob :: JobPhase -> Entity Job -> WorkerM ()
performJob phase (Entity jobID job@Job{..}) = do
  jrID <- inDataLayer $ createJobResult jobID phase

  projects <- getProjects
  project@Project{..} <- getProject projects jobProjectIdentifier
  let steps = case jobType of
        Build     -> projectBuildSteps
        PostBuild -> projectPostBuildSteps

  mErr <- flip catchError (return . Just) $ do
    forM_ steps $ \step@Step{..} -> do
      let handler e =
            throwError $ StepPluginError stepPlugin $ show (e :: SomeException)
      flip catch handler $ do
        eRes <- runStepPlugin project step phase job
        case eRes of
          Left  err   -> throwError $ StepPluginError stepPlugin err
          Right False -> throwError $ StepFailed      stepPlugin
          Right True  -> return ()
    return Nothing

  inDataLayer $ markJobResultOver jrID $ fmap show mErr

runStepPlugin :: Project -> Step -> JobPhase -> Job
              -> WorkerM (Either String Bool)
runStepPlugin project step phase job = do
    dir <- getJobDirectory project job
    let logFile = dir </> jobOutputFile phase (jobType job)
    fetchRepoIfMissing dir
    runStep dir logFile

  where
    fetchRepoIfMissing :: FilePath -> WorkerM ()
    fetchRepoIfMissing dir = do
      check <- liftIO $ doesDirectoryExist dir
      unless check $ do
        let source = projectSource project
        plugin <- getSourcePlugin $ sourcePlugin source
        eRes <- liftIO $ runPlugin $
          sourcePluginFetch plugin (sourceLocation source)
                            dir (sourceConfig source)
        either (throwError . SourcePluginError (sourcePlugin source))
               return eRes

    runStep :: FilePath -> FilePath -> WorkerM (Either String Bool)
    runStep dir logFile = do
      plugin <- getStepPlugin $ stepPlugin step
      let action = case phase of
            Run      -> stepPluginRun      plugin
            Rollback -> stepPluginRollback plugin

      env  <- getEnvironment
      eRes <- liftIO $ withFile logFile WriteMode $ \h -> do
        runWorker env jobWorkerCapability $
          runPlugin $ action dir h $ stepConfig step
      either throwError return eRes
