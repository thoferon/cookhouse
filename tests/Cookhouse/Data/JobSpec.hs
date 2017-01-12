module Cookhouse.Data.JobSpec where

import Control.Monad

import Data.Monoid
import Data.Time

import Cookhouse.Capabilities
import Cookhouse.Data.Job
import Cookhouse.Data.Project

import SpecHelpers

spec :: Spec
spec = do
  describe "stringToJobStatus/jobStatusToString" $ do
    it "is (kind of) isomorphic" $ do
      forM_ [ JobInQueue, JobInProgress, JobSuccess
            , JobFailure, JobRollbacked, JobAborted ] $ \status ->
        stringToJobStatus (jobStatusToString status) `shouldBe` Just status

  describe "stringToJobType/jobTypeToString" $ do
    it "is (kind of) isomorphic" $ do
      forM_ [Build, PostBuild] $ \typ ->
        stringToJobType (jobTypeToString typ) `shouldBe` Just typ

  let pastTime = UTCTime (ModifiedJulianDay 40000) 7825
      job      = Job Build JobSuccess "identifier" [] pastTime

  describe "getJob" $ do
    it "requires the capability CAGetJob" $ do
      test anonymousCapability mempty (getJob $ JobID 42)
        `shouldReturn` Left (PermissionError CAGetJob)

    it "gets the job with the given ID" $ do
      let mock = mockGet (JobID 42) job
      test (singleCapability CAGetJob) mock (getJob $ JobID 42)
        `shouldReturn` Right job

  describe "getJobsOfProject" $ do
    it "requires the capability CAGetJob" $ do
      test anonymousCapability mempty (getJobsOfProject "identifier")
        `shouldReturn` Left (PermissionError CAGetJob)

    it "gets the jobs with the given project identifier" $ do
      let mock = mockSelect (JobProjectIdentifier ==. "identifier")
                            (desc JobCreationTime) [Entity (JobID 42) job]
      test (singleCapability CAGetJob) mock (getJobsOfProject "identifier")
        `shouldReturn` Right [Entity (JobID 42) job]

  describe "getPendingJobs" $ do
    it "requires the capability CAGetJob" $ do
      test anonymousCapability mempty getPendingJobs
        `shouldReturn` Left (PermissionError CAGetJob)

    it "gets the jobs in queue or in progress" $ do
      let mock = mockSelect_ (JobStatus ==. JobInQueue
                              ||. JobStatus ==. JobInProgress)
                             [Entity (JobID 42) job]
      test (singleCapability CAGetJob) mock getPendingJobs
        `shouldReturn` Right [Entity (JobID 42) job]

  describe "getJobDependencies" $ do
    it "requires the capability CAGetJob" $ do
      test anonymousCapability mempty (getJobDependencies (JobID 43))
        `shouldReturn` Left (PermissionError CAGetJob)

    it "returns the jobs on which the given job depends" $ do
      let job' = job { jobDependencies = [JobID 42] }
          mock = mockGet (JobID 43) job'
                 `after` mockGetMany [JobID 42] [Entity (JobID 42) job]
      test (singleCapability CAGetJob) mock (getJobDependencies (JobID 43))
        `shouldReturn` Right [Entity (JobID 42) job]

  describe "editJob" $ do
    it "requires the capability CAEditJob" $ do
      test anonymousCapability mempty (editJob (JobID 1) JobSuccess)
        `shouldReturn` Left (PermissionError CAEditJob)

    it "edits the job status" $ do
      let mock = mockUpdate (JobID 42) (JobStatus =. JobRollbacked)
      test (singleCapability CAEditJob) mock
           (editJob (JobID 42) JobRollbacked) `shouldReturn` Right ()

  describe "createJob" $ do
    it "requires the capability CACreateJob" $ do
      test anonymousCapability mempty (createJob Build "some-project" [])
        `shouldReturn` Left (PermissionError CACreateJob)

    it "inserts a new Job in the database" $ do
      let job' = Job PostBuild JobInQueue "identifier" [JobID 42] someTime
          mock = mockInsert job' (JobID 1337)
      test (singleCapability CACreateJob) mock
           (createJob PostBuild "identifier" [JobID 42])
        `shouldReturn` Right (JobID 1337)

  describe "deleteJob" $ do
    it "requires the capability CADeleteJob" $ do
      test anonymousCapability mempty (deleteJob $ JobID 42)
        `shouldReturn` Left (PermissionError CADeleteJob)

    it "deletes the job AFTER its dependencies recursively" $ do
      let job11   = job { jobDependencies = [JobID 222] }
          job222  = job { jobDependencies = [JobID 3333] }
          job3333 = job { jobDependencies = [] }

          mock = do
            mockGet (JobID 11)   job11
            mockGet (JobID 222)  job222
            mockGet (JobID 3333) job3333
            mockDelete (JobID 3333)
            mockDelete (JobID 222)
            mockDelete (JobID 11)

      test (someCapabilities [CAGetJob, CADeleteJob]) mock
           (deleteJob $ JobID 11) `shouldReturn` Right ()
