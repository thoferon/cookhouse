module Cookhouse.Data.Plan where

import           Data.Aeson
import           Data.List
import           Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

import           Cookhouse.Data.Project
import           Cookhouse.Data.Internal
import           Cookhouse.Data.Types

data JobStatus
  = JobInQueue
  | JobInProgress
  | JobSuccess
  | JobFailure
  | JobRollbacked

instance PureFromField JobStatus where
  pureFromField = do
    ensureFieldType ["job_status"]
    bs <- ensureNotNull
    case stringToJobStatus $ BS.unpack bs of
      Just typ -> return typ
      Nothing -> fail $ "invalid job status: " ++ BS.unpack bs

instance ToField JobStatus where
  toField = toField . jobStatusToString

instance ToJSON JobStatus where
  toJSON = toJSON . jobStatusToString

stringToJobStatus :: String -> Maybe JobStatus
stringToJobStatus str = case str of
  "in-queue"    -> Just JobInQueue
  "in-progress" -> Just JobInProgress
  "success"     -> Just JobSuccess
  "failure"     -> Just JobFailure
  "rollbacked"  -> Just JobRollbacked
  _             -> Nothing

jobStatusToString :: JobStatus -> String
jobStatusToString typ = case typ of
  JobInQueue    -> "in-queue"
  JobInProgress -> "in-progress"
  JobSuccess    -> "success"
  JobFailure    -> "failure"
  JobRollbacked -> "rollbacked"

data JobType = Build | PostBuild

instance PureFromField JobType where
  pureFromField = do
    ensureFieldType ["job_type"]
    bs <- ensureNotNull
    case stringToJobType $ BS.unpack bs of
      Just typ -> return typ
      Nothing  -> fail $ "invalid job type: " ++ BS.unpack bs

instance ToField JobType where
  toField = toField . jobTypeToString

instance ToJSON JobType where
  toJSON = toJSON . jobTypeToString

stringToJobType :: String -> Maybe JobType
stringToJobType str = case str of
  "build"      -> Just Build
  "post-build" -> Just PostBuild
  _ -> Nothing

jobTypeToString :: JobType -> String
jobTypeToString typ = case typ of
  Build     -> "build"
  PostBuild -> "post-build"

data Job = Job
  { jobType              :: JobType
  , jobStatus            :: JobStatus
  , jobProjectIdentifier :: ProjectIdentifier
  , jobDependencies      :: [EntityID Job]
  , jobPath              :: Maybe FilePath
  }

instance PureFromRow Job where
  pureFromRow =
    Job <$> field <*> field <*> field <*> (fmap V.toList field) <*> field

instance ToRow Job where
  toRow (Job{..}) =
    [ toField jobType
    , toField jobStatus
    , toField jobProjectIdentifier
    , toField $ V.fromList jobDependencies
    , toField jobPath
    ]

instance Storable Job where
  data EntityID Job = JobID { unJobID :: Int64 }

  tableName  _ = "jobs"
  fieldNames _ = [ "type", "status", "project_identifier", "dependencies"
                 , "path"
                 ]

instance PureFromField (EntityID Job) where
  pureFromField = JobID <$> pureFromField

instance ToField (EntityID Job) where
  toField = toField . unJobID
