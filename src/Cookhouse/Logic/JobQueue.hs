module Cookhouse.Logic.JobQueue
  ( MetaJob(..)
  , getNextJob
  ) where

import Data.Monoid

import Cookhouse.Data.Job
import Cookhouse.Data.JobResult
import Cookhouse.Data.Types

data MetaJob = MetaJob JobPhase (Entity Job) deriving (Eq, Show)

getNextJob :: MonadDataLayer m => m (Maybe MetaJob)
getNextJob = getNextRollbackJob >>= maybe getNextRunJob (return . Just)

getNextRollbackJob :: MonadDataLayer m => m (Maybe MetaJob)
getNextRollbackJob = do
    ents <- findJobs $ JobStatus :== JobFailure <> Limit 1
    case ents of
      [] -> return Nothing
      ent@(Entity _ Job{..}) : _ ->
        (Just . MetaJob Rollback) <$> findCandidate ent jobDependencies

  where
    findCandidate :: MonadDataLayer m => Entity Job -> [EntityID Job]
                  -> m (Entity Job)
    findCandidate ent [] = return ent
    findCandidate ent (otherID : rest) = do
      other <- getJob otherID
      if jobStatus other /= JobRollbacked
        then findCandidate (Entity otherID other) $ jobDependencies other
        else findCandidate ent rest

getNextRunJob :: MonadDataLayer m => m (Maybe MetaJob)
getNextRunJob = do
    ents <- findJobs $ JobStatus :== JobInQueue <> Asc JobCreationTime
    findCandidate ents

  where
    findCandidate :: MonadDataLayer m => [Entity Job] -> m (Maybe MetaJob)
    findCandidate [] = return Nothing
    findCandidate (ent : ents) = do
      let deps = jobDependencies $ entityVal ent
      subs <- findJobs $
        JobIDField `BelongsTo` deps :&& JobStatus :/= JobSuccess
      case subs of
        [] -> return $ Just $ MetaJob Run ent
        _  -> findCandidate ents
