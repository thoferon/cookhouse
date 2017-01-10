module Cookhouse.Actions.JobResultActions where

import           Data.Maybe
import qualified Data.Text as T

import           Cookhouse.Actions.Types
import           Cookhouse.Data.JobResult
import           Cookhouse.Environment
import           Cookhouse.Logic.JobResultOutput

getJobResultOutputAction :: EntityID JobResult -> Maybe Integer -> Action T.Text
getJobResultOutputAction jobResultID mOffset = do
  let offset = fromMaybe 0 mOffset
  projects <- getProjects
  (jr, job, project) <- getJobResultOutputData projects jobResultID
  getJobResultOutput jr job project offset
