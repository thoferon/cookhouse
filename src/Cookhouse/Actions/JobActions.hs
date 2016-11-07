module Cookhouse.Actions.JobActions where

import Control.Monad.Trans

import System.Directory

import Cookhouse.Actions.Helpers
import Cookhouse.Data.Job
import Cookhouse.Data.JobResult
import Cookhouse.Data.Project
import Cookhouse.Logic.JobGeneration

getJobAction :: AppSpockAction ()
getJobAction =
  runPOF (paramPOF "job_id") $ \jobID -> do
    eRes <- inDataLayer $ (,,)
      <$> getJob jobID
      <*> getJobDependencies jobID
      <*> getJobResultsFor jobID

    withDataLayerResult eRes $ \(job, deps, results) -> do
      setStatus ok200
      json $ object
        [ "job"          .= job
        , "dependencies" .= deps
        , "results"      .= results
        ]

generateJobsAction :: AppSpockAction ()
generateJobsAction = runPOF (paramPOF "project_identifier") $ \identifier -> do
  projects <- getProjects
  eIDs <- inDataLayer $ generateJobs projects identifier
  withDataLayerResult eIDs $ \ids -> do
    setStatus created201
    json $ object [ "job_ids" .= ids ]

getPendingJobsAction :: AppSpockAction ()
getPendingJobsAction = do
  eJobs <- inDataLayer getPendingJobs
  withDataLayerResult eJobs $ \jobs -> do
    setStatus ok200
    json $ object [ "jobs" .= jobs ]

getJobsOfProjectAction :: AppSpockAction ()
getJobsOfProjectAction =
  runPOF (paramPOF "project_identifier") $ \identifier -> do
    withProject identifier $ \_ -> do
      eJobs <- inDataLayer $ getJobsOfProject identifier
      withDataLayerResult eJobs $ \jobs -> do
        setStatus ok200
        json $ object [ "jobs" .= jobs ]

deleteJobAction :: AppSpockAction ()
deleteJobAction = runPOF (paramPOF "job_id") $ \jobID -> do
  eJob <- inDataLayer $ do
    job <- getJob jobID
    deleteJob jobID
    return job

  withDataLayerResult eJob $ \job -> do
    projects <- getProjects
    case findProject projects $ jobProjectIdentifier job of
      Nothing -> return ()
      Just project -> do
        dir <- getJobDirectory project job
        liftIO $ removeDirectoryRecursive dir
    setStatus noContent204
