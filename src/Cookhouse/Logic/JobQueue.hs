module Cookhouse.Logic.JobQueue
  ( MetaJob(..)
  , getNextJob
  , abortUnneededJobs
  ) where

import Control.Monad

import Cookhouse.Data.Job
import Cookhouse.Data.JobResult
import Cookhouse.Data.Types

data MetaJob = MetaJob JobPhase (Entity Job) deriving (Eq, Show)

getNextJob :: MonadDataLayer m s => m (Maybe MetaJob)
getNextJob = getNextRollbackJob >>= maybe getNextRunJob (return . Just)

getNextRollbackJob :: MonadDataLayer m s => m (Maybe MetaJob)
getNextRollbackJob = do
    ents <- findJobs (JobStatus ==. JobFailure) (limit 1)
    case ents of
      [] -> return Nothing
      ent@(Entity _ Job{..}) : _ ->
        (Just . MetaJob Rollback) <$> findCandidate ent jobDependencies

  where
    findCandidate :: MonadDataLayer m s => Entity Job -> [EntityID Job]
                  -> m (Entity Job)
    findCandidate ent [] = return ent
    findCandidate ent (otherID : rest) = do
      other <- getJob otherID
      if jobStatus other /= JobRollbacked
        then findCandidate (Entity otherID other) $ jobDependencies other
        else findCandidate ent rest

getNextRunJob :: MonadDataLayer m s => m (Maybe MetaJob)
getNextRunJob = do
    ents <- findJobs (JobStatus ==. JobInQueue) (asc JobCreationTime)
    findCandidate ents

  where
    findCandidate :: MonadDataLayer m s => [Entity Job] -> m (Maybe MetaJob)
    findCandidate [] = return Nothing
    findCandidate (ent : ents) = do
      let deps = jobDependencies $ entityVal ent
      subs <- findJobs
        (EntityID `inList` deps &&. JobStatus /=. JobSuccess) mempty
      case subs of
        [] -> return $ Just $ MetaJob Run ent
        _  -> findCandidate ents

abortUnneededJobs :: MonadDataLayer m s => m ()
abortUnneededJobs = do
  ents <- findJobs (JobStatus ==. JobInQueue) mempty
  forM_ ents $ \(Entity jobID job) -> do
    jobs <- getManyJobs $ jobDependencies job
    let checkFailure = (`elem` [JobFailure, JobRollbacked, JobAborted])
                       . jobStatus . entityVal
    when (any checkFailure jobs) $ editJob jobID JobAborted
