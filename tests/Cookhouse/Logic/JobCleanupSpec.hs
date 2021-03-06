module Cookhouse.Logic.JobCleanupSpec where

import Control.Monad.Except

import Data.Monoid
import Data.Time

import Cookhouse.Data.Job
import Cookhouse.Logic.JobCleanup

import SpecHelpers

spec :: Spec
spec = do
  let defaultClauses = desc JobCreationTime <> desc EntityID

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
                       defaultClauses ents
            forM_ [25..30] $ mockDelete . JobID

      test jobWorkerCapability mock (deleteOldJobs "identifier")
        `shouldReturn` Right (reverse (take 24 ents))

    it "keeps more should it be necessary in order to keep at least 5 succeeded\
       \ jobs" $ do
      let (l,r) = splitAt 20 ents
          ents' = map (\(Entity jobID j) ->
                         Entity jobID j { jobStatus = JobFailure }) l ++ r
          mock  = do
            mockSelect (JobProjectIdentifier ==. "identifier")
                       defaultClauses ents'
            forM_ [26..30] $ mockDelete . JobID

      test jobWorkerCapability mock (deleteOldJobs "identifier")
        `shouldReturn` Right (reverse (take 25 ents'))
