module Cookhouse.Data.Plan
  (
  ) where

import Data.List
import Data.Maybe

import Cookhouse.Data.Project

data JobStatus
  = JobInQueue
  | JobInProgress
  | JobSuccess
  | JobFailure
  | JobRollbacked
  | JobNotNeeded

data Job dep
  = JobBuild     JobStatus String [dep] (Maybe FilePath)
  | JobPostBuild JobStatus dep [dep]
  | JobRollback  JobStatus dep

generateBuildJobs :: [Project] -> Project -> JobSpec
generateBuildJobs projects = go
  where
    go :: Project -> (JobSpec, JobSpec)
    go project =
      let deps      = catMaybes $ fmap findProject $ projectDependencies project
          (depBuilds, depPostBuilds) =

          identifier = projectIdentifier project
          build      = JobBuild     identifier depBuilds
          postBuild  = JobPostBuild build depPostBuilds
          rollback   = JobRollback  postBuild

      in rollback

    findProject :: String -> Maybe Project
    findProject identifier = find ((==identifier) . projectIdentifier) projects

    splitPairs :: [(a,b)] -> ([a], [b])
    splitPairs pairs = (map fst pairs, map snd pairs)
