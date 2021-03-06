{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cookhouse.Workers.JobWorker
  ( jobWorker
  ) where

import           Control.Concurrent
import           Control.Exception (AsyncException(..), fromException)
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.State

import           Data.Function
import           Data.List
import           Data.Maybe

import           System.Directory
import           System.FilePath
import           System.IO
import qualified System.Environment as E

import           Cookhouse.Capabilities
import           Cookhouse.Config
import           Cookhouse.Data.Job
import           Cookhouse.Data.JobResult
import           Cookhouse.Data.Project
import           Cookhouse.Data.Types hiding (get)
import           Cookhouse.Environment
import           Cookhouse.Errors
import           Cookhouse.Logic.JobCleanup
import           Cookhouse.Logic.JobQueue
import           Cookhouse.Logic.JobResultOutput
import           Cookhouse.Plugins.Types
import           Cookhouse.Workers.Helpers

{-
 - Worker management
 -}

type JobWorkerM = StateT [(ThreadId, EntityID Job, MVar ())] WorkerM

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
    stopAbortedJobs
    threads <- get
    when (length threads < maxJobCount) $ do
      mIdentifier <- spawnNextJob
      maybe (return ()) clearOldJobs mIdentifier
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
  forM_ threads $ \(i, _, mvar) -> do
    finished <- liftIO $ isJust <$> tryReadMVar mvar
    when finished $ removeThread i

stopAbortedJobs :: JobWorkerM ()
stopAbortedJobs = do
  threads <- get
  let ids = map (\(_,i,_) -> i) threads
  ents <- lift $ inDataLayer $
    findJobs (EntityID `inList` ids &&. JobStatus ==. JobAborted) mempty
  forM_ ents $ \(Entity jobID _) ->
    case find (\(_,i,_) -> i == jobID) threads of
      Nothing -> return () -- impossible
      Just (tid, _, _) -> liftIO $ killThread tid

clearOldJobs :: ProjectIdentifier -> JobWorkerM ()
clearOldJobs identifier = do
  ents <- lift $ inDataLayer $ deleteOldJobs identifier
  removeOldJobDirectories identifier ents

spawnNextJob :: JobWorkerM (Maybe ProjectIdentifier)
spawnNextJob = do
  mJob <- lift $ inDataLayer getNextJob
  case mJob of
    Nothing -> return Nothing
    Just (MetaJob typ ent@(Entity jobID Job{..})) -> do
      mvar <- liftIO newEmptyMVar
      env  <- getEnvironment

      i <- liftIO $ forkOS $ do
        eRes <- runWorker env jobWorkerCapability $ case typ of
          Run      -> runJob      ent
          Rollback -> rollbackJob ent
        case eRes of
          Left err -> hPutStrLn stderr $ "Job failed: " ++ show err
          Right () -> return ()
        putMVar mvar ()
      addThread i jobID mvar

      return $ Just jobProjectIdentifier

addThread :: ThreadId -> EntityID Job -> MVar () -> JobWorkerM ()
addThread i jobID mvar = state $ \pairs -> ((), (i, jobID, mvar) : pairs)

removeThread :: ThreadId -> JobWorkerM ()
removeThread i = state $ \pairs -> ((), filter (\(i',_,_) -> i' /= i) pairs)

{-
 - Proper job worker
 -}

runJob :: Entity Job -> WorkerM ()
runJob ent@(Entity jobID _) = do
  inDataLayer $ editJob jobID JobInProgress
  success <- performJob Run ent
  inDataLayer $ do
    editJob jobID $ if success then JobSuccess else JobFailure
    abortUnneededJobs

rollbackJob :: Entity Job -> WorkerM ()
rollbackJob ent@(Entity jobID _) = do
  inDataLayer $ editJob jobID JobRollbacked
  void $ performJob Rollback ent
  inDataLayer abortUnneededJobs

performJob :: JobPhase -> Entity Job -> WorkerM Bool
performJob phase jobEnt@(Entity jobID job@Job{..}) = do
  jobResultID <- inDataLayer $ upsertJobResult jobID phase

  mErr <- flip catchError (return . Just) $ do
    projects <- getProjects
    project@Project{..} <- getProject projects jobProjectIdentifier
    let steps = case jobType of
          Build     -> projectBuildSteps
          PostBuild -> projectPostBuildSteps

    dir <- prepareJobDirectory project job `catch` \e ->
      throwError $ IOError $ show (e :: SomeException)

    forM_ steps $ \step@Step{..} -> do
      let handler e = case fromException e of
            Just ThreadKilled -> throwM ThreadKilled
            _ -> throwError $ StepPluginError stepPlugin $ show e
      flip catch handler $ do
        eRes <- runStepPlugin step phase jobEnt dir
        case eRes of
          Left  err   -> throwError $ StepPluginError stepPlugin err
          Right False -> throwError $ StepFailed      stepPlugin
          Right True  -> return ()

    return Nothing

  inDataLayer $ markJobResultOver jobResultID $ fmap show mErr
  return $ isNothing mErr

runStepPlugin :: Step -> JobPhase -> Entity Job -> FilePath
              -> WorkerM (Either String Bool)
runStepPlugin Step{..} phase (Entity jobID job) dir = do
  let logFile = dir </> jobOutputFile phase (jobType job)

  plugin <- getStepPlugin stepPlugin
  let action = case phase of
        Run      -> stepPluginRun      plugin
        Rollback -> stepPluginRollback plugin

  envVars <- liftIO E.getEnvironment
  let cookhouseVars =
        [ ("PROJECT_ID", unProjectIdentifier (jobProjectIdentifier job))
        , ("JOB_ID",     show (unJobID jobID))
        ]
      envVars' = nubBy ((==) `on` fst) $ stepEnvVars ++ cookhouseVars ++ envVars
      dir'     = maybe dir (dir </>) stepSubdir

  env  <- getEnvironment
  eRes <- liftIO $ withFile logFile AppendMode $ \h -> do
    hSetBuffering h NoBuffering
    runWorker env jobWorkerCapability $
      runPlugin $ action dir' envVars' h stepConfig
  either throwError return eRes

prepareJobDirectory :: Project -> Job -> WorkerM FilePath
prepareJobDirectory project job = do
  dir <- getJobDirectory project job

  check <- liftIO $ doesDirectoryExist dir
  unless check $ do
    let source = projectSource project
    plugin <- getSourcePlugin $ sourcePlugin source
    eRes <- liftIO $ runPlugin $
      sourcePluginFetch plugin (sourceLocation source)
                        dir (sourceConfig source)
    either (throwError . SourcePluginError (sourcePlugin source))
           return eRes

    projects <- getProjects
    let vendorDir = dir </> "vendor"
    unless (projectDependencies project == []) $
      liftIO $ createDirectoryIfMissing True vendorDir

    depJobs <- inDataLayer $ getManyJobs $ jobDependencies job
    forM_ (projectDependencies project) $ \dep -> do
      depJob <- inDataLayer $ fmap entityVal $
        case find ((==dep) . jobProjectIdentifier . entityVal) depJobs of
          Nothing -> getLatestSucceededJob dep PostBuild
          Just e  -> return e
      depProject <- getProject projects dep
      depDir     <- getJobDirectory depProject depJob
      liftIO $ copyDirectory depDir $
        vendorDir </> unProjectIdentifier (jobProjectIdentifier depJob)

  return dir

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory src dest = do
  createDirectoryIfMissing True dest
  copyPermissions src dest

  names <- listDirectory src
  forM_ names $ \name -> do
    let srcPath  = src  </> name
        destPath = dest </> name
    checkDir <- doesDirectoryExist srcPath
    if checkDir
      then copyDirectory srcPath destPath
      else copyFileWithMetadata srcPath destPath
