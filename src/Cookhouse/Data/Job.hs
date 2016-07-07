module Cookhouse.Data.Job where

import           Data.Aeson
import           Data.Time
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V

import           Web.PathPieces

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

import           Cookhouse.Capabilities
import           Cookhouse.Data.Project
import           Cookhouse.Data.Types

data JobStatus
  = JobInQueue
  | JobInProgress
  | JobSuccess
  | JobFailure
  | JobRollbacked
  deriving (Eq, Show)

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

data JobType = Build | PostBuild deriving (Eq, Show)

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
  , jobCreationTime      :: UTCTime
  } deriving (Eq, Show)

jobDirectory :: Job -> FilePath
jobDirectory = formatTime defaultTimeLocale "%Y%m%d%H%M" . jobCreationTime

instance ToJSON Job where
  toJSON (Job{..}) = object
    [ "type"               .= jobType
    , "status"             .= jobStatus
    , "project_identifier" .= jobProjectIdentifier
    , "dependencies"       .= jobDependencies
    , "creation_time"      .= jobCreationTime
    ]

instance PureFromRow Job where
  pureFromRow = Job
    <$> field <*> field <*> field <*> (fmap V.toList field) <*> field

instance ToRow Job where
  toRow (Job{..}) =
    [ toField jobType
    , toField jobStatus
    , toField jobProjectIdentifier
    , toField $ V.fromList jobDependencies
    , toField jobCreationTime
    ]

instance Storable Job where
  data EntityID Job = JobID { unJobID :: Int64 } deriving (Eq, Show)

  tableName  _ = "jobs"
  fieldNames _ = [ "type", "status", "project_identifier", "dependencies"
                 , "creation_time"
                 ]

instance PureFromField (EntityID Job) where
  pureFromField = JobID <$> pureFromField

instance ToField (EntityID Job) where
  toField = toField . unJobID

instance ToJSON (EntityID Job) where
  toJSON = toJSON . unJobID

instance PathPiece (EntityID Job) where
  fromPathPiece = fmap JobID . fromPathPiece
  toPathPiece   = toPathPiece . unJobID

data JobField a where
  JobIDField           :: JobField (EntityID Job)
  JobStatus            :: JobField JobStatus
  JobProjectIdentifier :: JobField ProjectIdentifier
  JobCreationTime      :: JobField UTCTime

instance IsField Job JobField where
  toFieldName f = case f of
    JobIDField           -> idFieldName (Proxy :: Proxy Job)
    JobStatus            -> "status"
    JobProjectIdentifier -> "project_identifier"
    JobCreationTime      -> "creation_time"

getJob :: MonadDataLayer m => EntityID Job -> m Job
getJob jobID = do
  ensureAccess CAGetJob
  get jobID

getJobsOfProject :: MonadDataLayer m => ProjectIdentifier -> m [Entity Job]
getJobsOfProject identifier = findJobs $ JobProjectIdentifier :== identifier

getPendingJobs :: MonadDataLayer m => m [Entity Job]
getPendingJobs =
  findJobs $ JobStatus :== JobInQueue :|| JobStatus :== JobInProgress

findJobs :: MonadDataLayer m => GenericModifier JobField -> m [Entity Job]
findJobs mods = do
  ensureAccess CAGetJob
  select mods

editJob :: MonadDataLayer m => EntityID Job -> JobStatus -> m ()
editJob jobID status = do
  ensureAccess CAEditJob
  update jobID ["status" =. status]

createJob :: MonadDataLayer m => JobType -> ProjectIdentifier -> [EntityID Job]
          -> m (EntityID Job)
createJob typ identifier deps = do
  ensureAccess CACreateJob
  now <- getTime
  create Job
    { jobType              = typ
    , jobStatus            = JobInQueue
    , jobProjectIdentifier = identifier
    , jobDependencies      = deps
    , jobCreationTime      = now
    }

deleteJob :: MonadDataLayer m => EntityID Job -> m ()
deleteJob jobID = do
  ensureAccess CADeleteJob
  job <- getJob jobID
  mapM_ deleteJob $ jobDependencies job
  delete jobID
