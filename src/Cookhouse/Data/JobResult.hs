{-# LANGUAGE OverloadedLists #-}

module Cookhouse.Data.JobResult
  ( JobResult(..)
  , JobPhase(..)
  , EntityID(..)
  , JobResultProperty(..)
  , jobPhaseToString
  , stringToJobPhase
  , getJobResult
  , getJobResultsFor
  , createJobResult
  , upsertJobResult
  , markJobResultOver
  ) where

import           Data.Aeson
import           Data.Monoid
import           Data.Time
import qualified Data.ByteString.Char8 as BS

import           Servant.API

import           Cookhouse.Capabilities
import           Cookhouse.Data.Job
import           Cookhouse.Data.Types

data JobPhase = Run | Rollback deriving (Eq, Show)

instance FromRow PSQL One JobPhase where
  fromRow = pconsume `pbind` \(ColumnInfo{..}, Field{..}) ->
    case (colInfoType, fieldValue) of
      ("job_phase", Just bs) ->
        case stringToJobPhase $ BS.unpack bs of
        Just typ -> preturn typ
        Nothing  -> pfail $ "invalid job phase: " ++ BS.unpack bs
      (bs, Just _) -> pfail $ "invalid type for job phase: " ++ BS.unpack bs
      (_, Nothing) -> pfail "unexpected NULL for job phase"

instance ToRow PSQL One JobPhase where
  toRow backend = toRow backend . jobPhaseToString

instance ToJSON JobPhase where
  toJSON = toJSON . jobPhaseToString

jobPhaseToString :: JobPhase -> String
jobPhaseToString p = case p of
  Run      -> "run"
  Rollback -> "rollback"

stringToJobPhase :: String -> Maybe JobPhase
stringToJobPhase str = case str of
  "run"      -> Just Run
  "rollback" -> Just Rollback
  _          -> Nothing

data JobResult = JobResult
  { jrJobID     :: EntityID Job
  , jrError     :: Maybe String
  , jrPhase     :: JobPhase
  , jrStartTime :: UTCTime
  , jrEndTime   :: Maybe UTCTime
  } deriving (Eq, Show, Generic)

instance ToJSON JobResult where
  toJSON JobResult{..} = object $
    [ "job_id"     .= jrJobID
    , "phase"      .= jrPhase
    , "start_time" .= jrStartTime
    ] ++ maybe [] (pure . ("error"    .=)) jrError
      ++ maybe [] (pure . ("end_time" .=)) jrEndTime

instance FromRow PSQL Five JobResult
instance ToRow   PSQL Five JobResult

instance Storable PSQL One Five JobResult where
  data EntityID JobResult
    = JobResultID { unJobResultID :: Integer }
    deriving (Eq, Show, Generic)

  relation _ = Relation
    { relationName      = "job_results"
    , relationIDColumns = ["id"]
    , relationColumns   = ["job_id", "error", "phase", "start_time", "end_time"]
    }

instance FromRow PSQL One (EntityID JobResult)
instance ToRow   PSQL One (EntityID JobResult)

instance ToJSON (EntityID JobResult) where
  toJSON = toJSON . unJobResultID

instance FromHttpApiData (EntityID JobResult) where
  parseUrlPiece = fmap JobResultID . parseUrlPiece

data JobResultProperty backend n a where
  JobResultJobID     :: JobResultProperty PSQL One (EntityID Job)
  JobResultError     :: JobResultProperty PSQL One (Maybe String)
  JobResultPhase     :: JobResultProperty PSQL One JobPhase
  JobResultStartTime :: JobResultProperty PSQL One UTCTime
  JobResultEndTime   :: JobResultProperty PSQL One (Maybe UTCTime)

instance Property PSQL JobResult JobResultProperty where
  toColumns _ = \case
    JobResultJobID     -> ["job_id"]
    JobResultError     -> ["error"]
    JobResultPhase     -> ["phase"]
    JobResultStartTime -> ["start_time"]
    JobResultEndTime   -> ["end_time"]

getJobResult :: MonadDataLayer m s => EntityID JobResult -> m JobResult
getJobResult jobResultID = do
  ensureAccess CAGetJobResult
  get jobResultID

getJobResultsFor :: MonadDataLayer m s => EntityID Job -> m [Entity JobResult]
getJobResultsFor jobID = do
  ensureAccess CAGetJobResult
  select (JobResultJobID ==. jobID) (asc JobResultStartTime)

createJobResult :: MonadDataLayer m s => EntityID Job -> JobPhase
                -> m (EntityID JobResult)
createJobResult jobID phase = do
  ensureAccess CACreateJobResult
  now <- getTime
  insert JobResult
    { jrJobID      = jobID
    , jrError      = Nothing
    , jrPhase      = phase
    , jrStartTime  = now
    , jrEndTime    = Nothing
    }

-- In case a crashed instance has already created it
upsertJobResult :: MonadDataLayer m s => EntityID Job -> JobPhase
                -> m (EntityID JobResult)
upsertJobResult jobID phase = do
  ensureAccess CAGetJobResult
  ents <- select (JobResultJobID ==. jobID &&. JobResultPhase ==. phase)
                 (limit 1)
  case ents of
    [] -> createJobResult jobID phase
    Entity jrID _ : _ -> do
      ensureAccess CAEditJobResult
      now <- getTime
      update jrID (JobResultStartTime =. now <> JobResultEndTime =. Nothing)
      return jrID

markJobResultOver :: MonadDataLayer m s => EntityID JobResult -> Maybe String
                  -> m ()
markJobResultOver jrID mErr = do
  ensureAccess CAEditJobResult
  now <- getTime
  update jrID $ JobResultEndTime =. Just now <> JobResultError =. mErr
