module Cookhouse.Logic.JobQueueSpec where

import Data.Monoid

import Cookhouse.Data.Job
import Cookhouse.Data.JobResult
import Cookhouse.Logic.JobQueue

import SpecHelpers

spec :: Spec
spec = do
  let defaultClauses = desc JobCreationTime <> desc EntityID

  describe "getNextJob" $ do
    it "returns Nothing if nothing is available" $ do
      let mock = do
            mockSelect (JobStatus ==. JobFailure) (defaultClauses <> limit 1) []
            mockSelect (JobStatus ==. JobInQueue)
                       (defaultClauses <> asc JobCreationTime) []
      test jobWorkerCapability mock getNextJob
        `shouldReturn` Right Nothing

    it "returns a rollback job when needed" $ do
      let job = Job Build JobSuccess "identifier" [] someTime
          mock = mockSelect (JobStatus ==. JobFailure)
                            (defaultClauses <> limit 1) [Entity (JobID 1) job]
      test jobWorkerCapability mock getNextJob
        `shouldReturn` Right (Just (MetaJob Rollback (Entity (JobID 1) job)))

    it "returns a run job when available" $ do
      let job  = Job Build JobInQueue "identifier" [] someTime
          mock = do
            mockSelect (JobStatus ==. JobFailure) (defaultClauses <> limit 1) []
            mockSelect (JobStatus ==. JobInQueue)
                       (defaultClauses <> asc JobCreationTime)
                       [Entity (JobID 3) job]
            mockSelect (EntityID `inList` [] &&. JobStatus /=. JobSuccess)
                       defaultClauses []
      test jobWorkerCapability mock getNextJob
        `shouldReturn` Right (Just (MetaJob Run (Entity (JobID 3) job)))

  describe "abortUnneededJobs" $ do
    it "sets the job status to aborted when the dependencies have failed" $ do
      let job1 = Job Build JobInQueue "identifier" [JobID 11] someTime
          job2 = job1 { jobDependencies = [JobID 22] }
          job3 = job1 { jobDependencies = [JobID 33] }
          job4 = job1 { jobDependencies = [JobID 44] }
          job5 = job1 { jobDependencies = [JobID 55] }
          job6 = job1 { jobDependencies = [JobID 66] }

          job11 = job1 { jobStatus = JobFailure }
          job22 = job1 { jobStatus = JobRollbacked }
          job33 = job1 { jobStatus = JobAborted }
          job44 = job1 { jobStatus = JobInQueue }
          job55 = job1 { jobStatus = JobInProgress }
          job66 = job1 { jobStatus = JobSuccess }

          mock = do
            mockSelect (JobStatus ==. JobInQueue) defaultClauses
                       [ Entity (JobID 1) job1
                       , Entity (JobID 2) job2
                       , Entity (JobID 3) job3
                       , Entity (JobID 4) job4
                       , Entity (JobID 5) job5
                       , Entity (JobID 6) job6
                       ]
            mockSelect (EntityID `inList` [JobID 11]) defaultClauses
                       [Entity (JobID 11) job11]
            mockUpdate (JobID 1) (JobStatus =. JobAborted)
            mockSelect (EntityID `inList` [JobID 22]) defaultClauses
                       [Entity (JobID 22) job22]
            mockUpdate (JobID 2) (JobStatus =. JobAborted)
            mockSelect (EntityID `inList` [JobID 33]) defaultClauses
                       [Entity (JobID 33) job33]
            mockUpdate (JobID 3) (JobStatus =. JobAborted)
            mockSelect (EntityID `inList` [JobID 44]) defaultClauses
                       [Entity (JobID 44) job44]
            mockSelect (EntityID `inList` [JobID 55]) defaultClauses
                       [Entity (JobID 55) job55]
            mockSelect (EntityID `inList` [JobID 66]) defaultClauses
                       [Entity (JobID 66) job66]

      test jobWorkerCapability mock abortUnneededJobs `shouldReturn` Right ()
