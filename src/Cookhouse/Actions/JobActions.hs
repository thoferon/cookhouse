module Cookhouse.Actions.JobActions where

import Control.Monad

import Cookhouse.Actions.Types
import Cookhouse.Data.Job
import Cookhouse.Data.JobResult
import Cookhouse.Data.Project
import Cookhouse.Data.Types
import Cookhouse.Environment
import Cookhouse.Logic.JobGeneration

data JobInfo = JobInfo (Entity Job) [Entity Job] [Entity JobResult]

instance ToJSON JobInfo where
  toJSON (JobInfo job deps results) = object
    [ "job"          .= job
    , "dependencies" .= deps
    , "results"      .= results
    ]

getJobAction :: EntityID Job -> Action JobInfo
getJobAction jobID = JobInfo
  <$> (Entity jobID <$> getJob jobID)
  <*> getJobDependencies jobID
  <*> getJobResultsFor jobID

getPendingJobsAction :: Action [Entity Job]
getPendingJobsAction = getPendingJobs

getJobsOfProjectAction :: ProjectIdentifier -> Action [Entity Job]
getJobsOfProjectAction = getJobsOfProject

deleteJobAction :: EntityID Job -> Action NoContent
deleteJobAction jobID = do
  deleteJob jobID
  return NoContent

generateJobsAction :: ProjectIdentifier -> Action [Entity Job]
generateJobsAction identifier = do
  projects <- getProjects
  ids <- generateJobs projects identifier
  getManyJobs ids

abortJobAction :: EntityID Job -> Action NoContent
abortJobAction jobID = do
  job <- get jobID
  when (jobStatus job `elem` [JobInQueue, JobInProgress]) $
    editJob jobID JobAborted
  return NoContent
