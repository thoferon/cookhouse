module Cookhouse.Logic.JobResultOutput
  ( jobOutputFile
  , getJobResultOutputData
  , getJobResultOutput
  ) where

import           Control.Monad.Trans

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           System.Directory
import           System.FilePath
import           System.IO

import           Cookhouse.Data.Job
import           Cookhouse.Data.JobResult
import           Cookhouse.Data.Project
import           Cookhouse.Data.Types hiding (offset)
import           Cookhouse.Environment

jobOutputFile :: JobPhase -> JobType -> FilePath
jobOutputFile phase jobType = case (phase, jobType) of
  (Run, Build)     -> "cookhouse-build.log"
  (Run, PostBuild) -> "cookhouse-post-build.log"
  (Rollback, _)    -> "cookhouse-rollback.log"

getJobResultOutputData :: MonadDataLayer m s => [Project] -> EntityID JobResult
                       -> m (JobResult, Job, Project)
getJobResultOutputData projects jobResultID = do
  jr@JobResult{..} <- getJobResult jobResultID
  job@Job{..}      <- getJob jrJobID
  project          <- getProject projects jobProjectIdentifier
  return (jr, job, project)

getJobResultOutput :: (HasEnvironment m, MonadIO m) => JobResult -> Job
                   -> Project -> Integer -> m T.Text
getJobResultOutput JobResult{..} job@Job{..} project offset = do
  dir <- getJobDirectory project job
  let path = dir </> jobOutputFile jrPhase jobType

  check <- liftIO $ doesFileExist path
  if check
    then
      liftIO $ withFile path ReadMode $ \h -> do
        hSeek h AbsoluteSeek offset
        T.hGetContents h
    else return ""
