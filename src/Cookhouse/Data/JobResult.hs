module Cookhouse.Data.JobResult
  ( JobResult(..)
  , JobPhase(..)
  , EntityID(..)
  , jobPhaseToString
  , stringToJobPhase
  , getJobResult
  , getJobResultsFor
  , createJobResult
  , markJobResultOver
  ) where

import           Data.Aeson
import           Data.Monoid
import           Data.Time
import qualified Data.ByteString.Char8 as BS

import           Web.PathPieces

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow

import           Cookhouse.Capabilities
import           Cookhouse.Data.Job
import           Cookhouse.Data.Types
import           Cookhouse.Errors

data JobPhase = Run | Rollback deriving (Eq, Show)

instance PureFromField JobPhase where
  pureFromField = do
    ensureFieldType ["job_phase"]
    bs <- ensureNotNull
    case stringToJobPhase $ BS.unpack bs of
      Just p  -> return p
      Nothing -> fail $ "invalid job phase: " ++ show bs

instance ToField JobPhase where
  toField = toField . jobPhaseToString

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
  } deriving (Eq, Show)

instance ToJSON JobResult where
  toJSON JobResult{..} = object $
    [ "job_id"     .= jrJobID
    , "phase"      .= jrPhase
    , "start_time" .= jrStartTime
    ] ++ maybe [] (pure . ("error"    .=)) jrError
      ++ maybe [] (pure . ("end_time" .=)) jrEndTime

instance PureFromRow JobResult where
  pureFromRow = JobResult <$> field <*> field <*> field <*> field <*> field

instance ToRow JobResult where
  toRow JobResult{..} =
    [ toField jrJobID
    , toField jrError
    , toField jrPhase
    , toField jrStartTime
    , toField jrEndTime
    ]

instance Storable JobResult where
  data EntityID JobResult
    = JobResultID { unJobResultID :: Int64 }
    deriving (Eq, Show)

  tableName  _ = "job_results"
  fieldNames _ = ["job_id", "error", "phase", "start_time", "end_time"]

instance PureFromField (EntityID JobResult) where
  pureFromField = fmap JobResultID pureFromField

instance ToField (EntityID JobResult) where
  toField = toField . unJobResultID

instance ToJSON (EntityID JobResult) where
  toJSON = toJSON . unJobResultID

instance PathPiece (EntityID JobResult) where
  toPathPiece = toPathPiece . unJobResultID
  fromPathPiece = fmap JobResultID . fromPathPiece

getJobResult :: MonadDataLayer m => EntityID JobResult -> m JobResult
getJobResult jobResultID = do
  ensureAccess CAGetJobResult
  get jobResultID

getJobResultsFor :: MonadDataLayer m => EntityID Job -> m [Entity JobResult]
getJobResultsFor jobID = do
  ensureAccess CAGetJobResult
  select $ "job_id" ==. jobID <> asc "start_time"

createJobResult :: MonadDataLayer m => EntityID Job -> JobPhase
                -> m (EntityID JobResult)
createJobResult jobID phase = do
  ensureAccess CACreateJobResult
  now <- getTime
  create JobResult
    { jrJobID      = jobID
    , jrError      = Nothing
    , jrPhase      = phase
    , jrStartTime  = now
    , jrEndTime    = Nothing
    }

markJobResultOver :: MonadDataLayer m => EntityID JobResult -> Maybe String
                  -> m ()
markJobResultOver jrID mErr = do
  ensureAccess CAEditJobResult
  now <- getTime
  update jrID ["end_time" =. Just now, "error" =. mErr]
