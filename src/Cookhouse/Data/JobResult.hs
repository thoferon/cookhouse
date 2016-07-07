module Cookhouse.Data.JobResult where

import           Data.Binary
import           Data.Monoid
import           Data.Time
import qualified Data.ByteString.Char8 as BS

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

jobPhaseToString :: JobPhase -> String
jobPhaseToString p = case p of
  Run      -> "run"
  Rollback -> "rollback"

stringToJobPhase :: String -> Maybe JobPhase
stringToJobPhase str = case str of
  "run"      -> Just Run
  "rollback" -> Just Rollback
  _          -> Nothing

jobOutputFile :: JobPhase -> Job -> FilePath
jobOutputFile phase Job{..} = case (phase, jobType) of
  (Run, Build)     -> "cookhouse-build.log"
  (Run, PostBuild) -> "cookhouse-post-build.log"
  (Rollback, _)    -> "cookhouse-rollback.log"

data JobResult = JobResult
  { jrJobID      :: EntityID Job
  , jrError      :: Maybe CookhouseError
  , jrPhase      :: JobPhase
  , jrEndTime    :: UTCTime
  } deriving (Eq, Show)

instance PureFromRow JobResult where
  pureFromRow = JobResult
    <$> field <*> (fmap decode <$> field) <*> field <*> field

instance ToRow JobResult where
  toRow JobResult{..} =
    [ toField jrJobID
    , toField $ fmap encode jrError
    , toField jrPhase
    , toField jrEndTime
    ]

instance Storable JobResult where
  data EntityID JobResult
    = JobResultID { unJobResultID :: Int64 }
    deriving (Eq, Show)

  tableName  _ = "job_results"
  fieldNames _ = ["job_id", "error", "phase", "end_time"]

instance PureFromField (EntityID JobResult) where
  pureFromField = fmap JobResultID pureFromField

instance ToField (EntityID JobResult) where
  toField = toField . unJobResultID

getJobResultsFor :: MonadDataLayer m => EntityID Job -> m [Entity JobResult]
getJobResultsFor jobID = do
  ensureAccess CAGetJobResult
  select $ "job_id" ==. jobID <> asc "end_time"

createJobResult :: MonadDataLayer m => EntityID Job -> Maybe CookhouseError
                -> JobPhase -> m (EntityID JobResult)
createJobResult jobID mErr phase = do
  ensureAccess CACreateJobResult
  now <- getTime
  create JobResult
    { jrJobID      = jobID
    , jrError      = mErr
    , jrPhase      = phase
    , jrEndTime    = now
    }
