module Cookhouse.Actions.JobResultActions where

import Data.Maybe

import Cookhouse.Actions.Helpers
import Cookhouse.Logic.JobResultOutput

getJobResultOutputAction :: AppSpockAction ()
getJobResultOutputAction = do
  let pof = (,) <$> paramPOF "job_result_id" <*> paramPOFMaybe "offset"
  runPOF pof $ \(jobResultID, mOffset) -> do
    let offset = fromMaybe 0 mOffset
    projects <- getProjects
    eData <- inDataLayer $ getJobResultOutputData projects jobResultID
    withDataLayerResult eData $ \(jr, job, project) -> do
      setStatus ok200
      out <- getJobResultOutput jr job project offset
      text out
