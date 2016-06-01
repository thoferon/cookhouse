module Cookhouse.Data.JobSpec where

import Control.Monad

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
            , JobFailure, JobRollbacked ] $ \status ->
        stringToJobStatus (jobStatusToString status) `shouldBe` Just status

  describe "stringToJobType/jobTypeToString" $ do
    it "is (kind of) isomorphic" $ do
      forM_ [Build, PostBuild] $ \typ ->
        stringToJobType (jobTypeToString typ) `shouldBe` Just typ

  let pastTime  = UTCTime (ModifiedJulianDay 40000) 7825
      job       = Job Build JobSuccess "identifier" [] Nothing pastTime
      jobFields = [ integer 42, PureField "job_type" (Just "build")
                  , PureField "job_status" (Just "success")
                  , varchar "identifier" , emptyArray "integer"
                  , nullValue "varchar" , timestamp' pastTime ]

  describe "getJob" $ do
    let fakeSelect "jobs" _ mods
          | wmodTemplate (selModWhere mods) == "id = ?"
            && wmodValues (selModWhere mods) == [toField (42 :: Int)] =
            return [jobFields]
        fakeSelect tbl cols mods = failEmulator $
          "Can't handle SELECT: " ++ show (tbl, cols, mods)

        emulator = mempty { deSelect = fakeSelect }

    it "requires the capability CAGetJob" $ do
      test anonymousCapability mempty (getJob $ JobID 42)
        `shouldBe` Left (AccessError CAGetJob)

    it "gets the job with the given ID" $ do
      test (singleCapability CAGetJob) emulator (getJob $ JobID 42)
        `shouldBe` Right job

  describe "getJobsOfProject" $ do
    let fakeSelect "jobs" _ mods
          | wmodTemplate (selModWhere mods) == "project_identifier = ?"
            && wmodValues (selModWhere mods)
                 == [toField ("identifier" :: ProjectIdentifier)] =
            return [jobFields]
        fakeSelect tbl cols mods = failEmulator $
          "Can't handle SELECT: " ++ show (tbl, cols, mods)

        emulator = mempty { deSelect = fakeSelect }

    it "requires the capability CAGetJob" $ do
      test anonymousCapability mempty (getJobsOfProject "identifier")
        `shouldBe` Left (AccessError CAGetJob)

    it "gets the jobs with the given project identifier" $ do
      test (singleCapability CAGetJob) emulator (getJobsOfProject "identifier")
        `shouldBe` Right [Entity (JobID 42) job]

  describe "getPendingJobs" $ do
    let fakeSelect "jobs" _ mods
          | wmodTemplate (selModWhere mods)
              == "(status = ?) OR (status = ?)"
            && wmodValues (selModWhere mods)
                 == [toField JobInQueue, toField JobInProgress] =
            return [jobFields]
        fakeSelect tbl cols mods = failEmulator $
          "Can't handle SELECT: " ++ show (tbl, cols, mods)

        emulator = mempty { deSelect = fakeSelect }

    it "requires the capability CAGetJob" $ do
      test anonymousCapability mempty getPendingJobs
        `shouldBe` Left (AccessError CAGetJob)

    it "gets the jobs in queue or in progress" $ do
      test (singleCapability CAGetJob) emulator (getPendingJobs)
        `shouldBe` Right [Entity (JobID 42) job]

  describe "createJob" $ do
    it "requires the capability CACreateJob" $ do
      test anonymousCapability mempty (createJob Build "some-project" [])
        `shouldBe` Left (AccessError CACreateJob)

    it "inserts a new Job in the database" $ do
      let job' = Job PostBuild JobInQueue "identifier" [JobID 42, JobID 1337]
                     Nothing someTime
          fakeInsert "jobs" _ "id" vals
            | [toRow job'] == map toRow vals = return [[integer 9999]]
          fakeInsert tbl cols ret vals = failEmulator $
            "Wrong request: " ++ show (tbl, cols, ret, map toRow vals)
          emulator = mempty { deInsert = fakeInsert }

      test (singleCapability CACreateJob) emulator
           (createJob PostBuild "identifier" [JobID 42, JobID 1337])
        `shouldBe` Right (JobID 9999)
