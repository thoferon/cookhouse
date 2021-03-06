{-# LANGUAGE OverloadedLists #-}

module Cookhouse.Data.Job
  ( Job(..)
  , JobStatus(..)
  , stringToJobStatus
  , jobStatusToString
  , JobType(..)
  , stringToJobType
  , jobTypeToString
  , EntityID(..)
  , JobProperty(..)
  , jobDirectory
  , getJob
  , getManyJobs
  , getJobsOfProject
  , getPendingJobs
  , getJobDependencies
  , getLatestSucceededJob
  , findJobs
  , editJob
  , createJob
  , deleteJob
  ) where

import           Data.Aeson
import           Data.Monoid
import           Data.Time
import qualified Data.ByteString.Char8 as BS

import           Servant.API

import           Cookhouse.Capabilities
import           Cookhouse.Data.Project
import           Cookhouse.Data.Types

data JobStatus
  = JobInQueue
  | JobInProgress
  | JobSuccess
  | JobFailure
  | JobRollbacked
  | JobAborted
  deriving (Eq, Show)

instance FromRow PSQL One JobStatus where
  fromRow = pconsume `pbind` \(ColumnInfo{..}, Field{..}) ->
    case (colInfoType, fieldValue) of
      ("job_status", Just bs) ->
        case stringToJobStatus $ BS.unpack bs of
          Just typ -> preturn typ
          Nothing  -> pfail $ "invalid job status: " ++ BS.unpack bs
      (bs, Just _) -> pfail $ "invalid type for job status: " ++ BS.unpack bs
      (_, Nothing) -> pfail "unexpected NULL for job status"

instance ToRow PSQL One JobStatus where
  toRow backend = toRow backend . jobStatusToString

instance ToJSON JobStatus where
  toJSON = toJSON . jobStatusToString

stringToJobStatus :: String -> Maybe JobStatus
stringToJobStatus str = case str of
  "in-queue"    -> Just JobInQueue
  "in-progress" -> Just JobInProgress
  "success"     -> Just JobSuccess
  "failure"     -> Just JobFailure
  "rollbacked"  -> Just JobRollbacked
  "aborted"     -> Just JobAborted
  _             -> Nothing

jobStatusToString :: JobStatus -> String
jobStatusToString typ = case typ of
  JobInQueue    -> "in-queue"
  JobInProgress -> "in-progress"
  JobSuccess    -> "success"
  JobFailure    -> "failure"
  JobRollbacked -> "rollbacked"
  JobAborted    -> "aborted"

data JobType = Build | PostBuild deriving (Eq, Show)

instance FromRow PSQL One JobType where
  fromRow = pconsume `pbind` \(ColumnInfo{..}, Field{..}) ->
    case (colInfoType, fieldValue) of
      ("job_type", Just bs) ->
        case stringToJobType $ BS.unpack bs of
        Just typ -> preturn typ
        Nothing  -> pfail $ "invalid job type: " ++ BS.unpack bs
      (bs, Just _) -> pfail $ "invalid type for job type: " ++ BS.unpack bs
      (_, Nothing) -> pfail "unexpected NULL for job type"

instance ToRow PSQL One JobType where
  toRow backend = toRow backend . jobTypeToString

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
  } deriving (Eq, Show, Generic)

jobDirectory :: Job -> FilePath
jobDirectory = formatTime defaultTimeLocale "%Y%m%d%H%M%S" . jobCreationTime

instance ToJSON Job where
  toJSON (Job{..}) = object
    [ "type"               .= jobType
    , "status"             .= jobStatus
    , "project_identifier" .= jobProjectIdentifier
    , "dependencies"       .= jobDependencies
    , "creation_time"      .= jobCreationTime
    ]

instance FromRow PSQL Five Job
instance ToRow   PSQL Five Job

instance Storable PSQL One Five Job where
  data EntityID Job = JobID { unJobID :: Integer } deriving (Eq, Show, Generic)

  relation  _ = Relation
    { relationName      = "jobs"
    , relationIDColumns = ["id"]
    , relationColumns   = [ "type", "status", "project_identifier"
                          , "dependencies", "creation_time" ]
    }

instance FromRow PSQL One (EntityID Job)
instance ToRow   PSQL One (EntityID Job)

instance ToJSON (EntityID Job) where
  toJSON = toJSON . unJobID

instance FromHttpApiData (EntityID Job) where
  parseUrlPiece = fmap JobID . parseUrlPiece

data JobProperty backend n a where
  JobType              :: JobProperty PSQL One JobType
  JobStatus            :: JobProperty PSQL One JobStatus
  JobProjectIdentifier :: JobProperty PSQL One ProjectIdentifier
  JobCreationTime      :: JobProperty PSQL One UTCTime

instance Property PSQL Job JobProperty where
  toColumns _ = \case
    JobType              -> ["type"]
    JobStatus            -> ["status"]
    JobProjectIdentifier -> ["project_identifier"]
    JobCreationTime      -> ["creation_time"]

getJob :: MonadDataLayer m s => EntityID Job -> m Job
getJob jobID = do
  ensureAccess CAGetJob
  get jobID

getManyJobs :: MonadDataLayer m s => [EntityID Job] -> m [Entity Job]
getManyJobs jobIDs = findJobs (EntityID `inList` jobIDs) mempty

getJobsOfProject :: MonadDataLayer m s => ProjectIdentifier -> m [Entity Job]
getJobsOfProject identifier =
  findJobs (JobProjectIdentifier ==. identifier) mempty

getPendingJobs :: MonadDataLayer m s => m [Entity Job]
getPendingJobs =
  findJobs (JobStatus ==. JobInQueue ||. JobStatus ==. JobInProgress) mempty

getJobDependencies :: MonadDataLayer m s => EntityID Job -> m [Entity Job]
getJobDependencies jobID = do
  Job{..} <- getJob jobID
  getMany jobDependencies

getLatestSucceededJob :: MonadDataLayer m s => ProjectIdentifier -> JobType
                      -> m (Entity Job)
getLatestSucceededJob identifier typ = do
  jobs <- findJobs (JobProjectIdentifier ==. identifier
                    &&. JobStatus ==. JobSuccess &&. JobType ==. typ)
                   (desc JobCreationTime <> limit 1)
  case jobs of
    [] -> throwSeakaleError EntityNotFoundError
    job : _ -> return job

findJobs :: MonadDataLayer m s => Condition PSQL Job -> SelectClauses PSQL Job
         -> m [Entity Job]
findJobs cond clauses = do
  ensureAccess CAGetJob
  select cond (desc JobCreationTime <> desc EntityID <> clauses)

editJob :: MonadDataLayer m s => EntityID Job -> JobStatus -> m ()
editJob jobID status = do
  ensureAccess CAEditJob
  update jobID $ JobStatus =. status

createJob :: MonadDataLayer m s => JobType -> ProjectIdentifier
          -> [EntityID Job] -> m (EntityID Job)
createJob typ identifier deps = do
  ensureAccess CACreateJob
  now <- getTime
  insert Job
    { jobType              = typ
    , jobStatus            = JobInQueue
    , jobProjectIdentifier = identifier
    , jobDependencies      = deps
    , jobCreationTime      = now
    }

deleteJob :: MonadDataLayer m s => EntityID Job -> m ()
deleteJob jobID = do
  ensureAccess CADeleteJob
  delete jobID
