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

          statusFailure =
            JobStatus `inList` [JobFailure, JobRollbacked, JobAborted]
          mock = do
            mockSelect (JobStatus ==. JobInQueue)
                       (defaultClauses <> asc EntityID)
                       [ Entity (JobID 1) job1
                       , Entity (JobID 2) job2
                       , Entity (JobID 3) job3
                       , Entity (JobID 4) job4
                       , Entity (JobID 5) job5
                       , Entity (JobID 6) job6
                       ]
            mockCount  (EntityID `inList` [JobID 11] &&. statusFailure) 1
            mockUpdate (JobID 1) (JobStatus =. JobAborted)
            mockCount  (EntityID `inList` [JobID 22] &&. statusFailure) 1
            mockUpdate (JobID 2) (JobStatus =. JobAborted)
            mockCount  (EntityID `inList` [JobID 33] &&. statusFailure) 1
            mockUpdate (JobID 3) (JobStatus =. JobAborted)
            mockCount  (EntityID `inList` [JobID 44] &&. statusFailure) 0
            mockCount  (EntityID `inList` [JobID 55] &&. statusFailure) 0
            mockCount  (EntityID `inList` [JobID 66] &&. statusFailure) 0

      test jobWorkerCapability mock abortUnneededJobs `shouldReturn` Right ()
