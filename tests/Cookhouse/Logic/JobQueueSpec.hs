module Cookhouse.Logic.JobQueueSpec where

import Cookhouse.Data.Job
import Cookhouse.Data.JobResult
import Cookhouse.Logic.JobQueue

import SpecHelpers

spec :: Spec
spec = do
  return ()
  describe "getNextJob" $ do
    it "returns Nothing if nothing is available" $ do
      let mock = do
            mockSelect (JobStatus ==. JobFailure) (limit 1) []
            mockSelect (JobStatus ==. JobInQueue) (asc JobCreationTime) []
      test jobWorkerCapability mock getNextJob
        `shouldBe` Right Nothing

    it "returns a rollback job when needed" $ do
      let job = Job Build JobSuccess "identifier" [] someTime
          mock = mockSelect (JobStatus ==. JobFailure) (limit 1)
                            [Entity (JobID 1) job]
      test jobWorkerCapability mock getNextJob
        `shouldBe` Right (Just (MetaJob Rollback (Entity (JobID 1) job)))

    it "returns a run job when available" $ do
      let job  = Job Build JobInQueue "identifier" [] someTime
          mock = do
            mockSelect (JobStatus ==. JobFailure) (limit 1) []
            mockSelect (JobStatus ==. JobInQueue) (asc JobCreationTime)
                       [Entity (JobID 3) job]
            mockSelect_ (EntityID `inList` [] &&. JobStatus /=. JobSuccess) []
      test jobWorkerCapability mock getNextJob
        `shouldBe` Right (Just (MetaJob Run (Entity (JobID 3) job)))
