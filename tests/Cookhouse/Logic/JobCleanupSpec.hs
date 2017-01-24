module Cookhouse.Logic.JobCleanupSpec where

import Control.Monad.Except
import Database.Seakale.Types

import Control.Monad

import Data.List
import Data.Time

import Cookhouse.Data.Job
import Cookhouse.Logic.JobCleanup

import SpecHelpers

spec :: Spec
spec = do
  describe "deleteOldJobs" $ do
    let next (Entity (JobID i) j) =
          let job' =
                j { jobCreationTime = addUTCTime (-1) (jobCreationTime j) }
          in Entity (JobID (i + 1)) job'
        job  = Job Build JobSuccess "identifier" [] someTime
        ents = take 30 $ iterate next (Entity (JobID 1) job)

    it "keeps the 24 newest jobs and delete the others" $ do
      let mock = do
            mockSelect (JobProjectIdentifier ==. "identifier")
                       (desc JobCreationTime) ents
            forM_ [25..30] $ \i -> do
              mockGet (JobID i) (entityVal (ents `genericIndex` (i - 1)))
              mockDelete (JobID i)

      test jobWorkerCapability mock (deleteOldJobs "identifier")
        `shouldReturn` Right (reverse (take 24 ents))

    it "keeps more should it be necessary in order to keep at least 5 succeeded\
       \ jobs" $ do
      let (l,r) = splitAt 20 ents
          ents' = map (\(Entity jobID j) ->
                         Entity jobID j { jobStatus = JobFailure }) l ++ r
          mock  = do
            mockSelect (JobProjectIdentifier ==. "identifier")
                       (desc JobCreationTime) ents'
            forM_ [26..30] $ \i -> do
              mockGet (JobID i) (entityVal (ents `genericIndex` (i - 1)))
              mockDelete (JobID i)

      test jobWorkerCapability mock (deleteOldJobs "identifier")
        `shouldReturn` Right (reverse (take 25 ents'))

    it "doesn't fail if a job has already been deleted by dependencies" $ do
      let ent@(Entity _ job) = ents !! 25
          ent'  = ent { entityVal = job { jobDependencies = [JobID 25] } }
          ents' = take 25 ents ++ [ent'] ++ drop 26 ents
          mock  = do
            mockSelect (JobProjectIdentifier ==. "identifier")
                       (desc JobCreationTime) ents'
            mockGet (JobID 25) (entityVal (head ents))
            mockDelete (JobID 25)
            mockGet (JobID 26) (entityVal ent')
            mockFailingGet (JobID 25) EntityNotFoundError
            mockDelete (JobID 26)
            forM_ [27..30] $ \i -> do
              mockGet (JobID i) (entityVal (ents `genericIndex` (i - 1)))
              mockDelete (JobID i)

      test jobWorkerCapability mock (deleteOldJobs "identifier")
        `shouldReturn` Right (reverse (take 24 ents'))
