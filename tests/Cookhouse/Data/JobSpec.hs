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
      job       = Job Build JobSuccess "identifier" [] pastTime
      jobFields = [ integer 42, PureField "job_type" (Just "build")
                  , PureField "job_status" (Just "success")
                  , varchar "identifier", emptyArray "int4"
                  , timestamp' pastTime ]

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

  describe "editJob" $ do
    let fakeUpdate "jobs" [("status", action)] mods
          | action == toField ("rollbacked" :: String)
            && wmodTemplate (selModWhere mods) == "id = ?"
            && wmodValues (selModWhere mods) == [toField (42 :: Int)] = return 1
        fakeUpdate tbl pairs mods = failEmulator $
          "Can't handle UPDATE: " ++ show (tbl, pairs, mods)

        emulator = mempty { deUpdate = fakeUpdate }

    it "requires the capability CAEditJob" $ do
      test anonymousCapability mempty (editJob (JobID 1) JobSuccess)
        `shouldBe` Left (AccessError CAEditJob)

    it "edits the job status" $ do
      test (singleCapability CAEditJob) emulator
           (editJob (JobID 42) JobRollbacked) `shouldBe` Right ()

  describe "createJob" $ do
    it "requires the capability CACreateJob" $ do
      test anonymousCapability mempty (createJob Build "some-project" [])
        `shouldBe` Left (AccessError CACreateJob)

    it "inserts a new Job in the database" $ do
      let job' = Job PostBuild JobInQueue "identifier" [JobID 42, JobID 1337]
                     someTime
          fakeInsert "jobs" _ "id" vals
            | [toRow job'] == map toRow vals = return [[integer 9999]]
          fakeInsert tbl cols ret vals = failEmulator $
            "Can't handle INSERT: " ++ show (tbl, cols, ret, map toRow vals)
          emulator = mempty { deInsert = fakeInsert }

      test (singleCapability CACreateJob) emulator
           (createJob PostBuild "identifier" [JobID 42, JobID 1337])
        `shouldBe` Right (JobID 9999)

  describe "deleteJob" $ do
    it "requires the capability CADeleteJob" $ do
      test anonymousCapability mempty (deleteJob $ JobID 42)
        `shouldBe` Left (AccessError CADeleteJob)

    it "deletes the job AFTER its dependencies recursively" $ do
      let mkJobRows jobID deps =
            [ integer jobID, PureField "job_type" (Just "build")
            , PureField "job_status" (Just "in-queue"), varchar "identifier"
            , array "int4" deps, timestamp' someTime ]

          fakeSelect "jobs" _ mods
            | wmodValues (selModWhere mods) == [toField (11 :: Int)] =
                return [mkJobRows 11 "{222}"]
            | wmodValues (selModWhere mods) == [toField (222 :: Int)] =
                return [mkJobRows 222 "{3333}"]
            | wmodValues (selModWhere mods) == [toField (3333 :: Int)] =
                return [mkJobRows 3333 "{}"]
          fakeSelect tbl cols mods = failEmulator $
            "Can't handle SELECT: " ++ show (tbl, cols, mods)
          selectEmulator = mempty { deSelect = fakeSelect }

          mkFakeDelete jobID next "jobs" mods
            | wmodTemplate (selModWhere mods) == "id = ?"
              && wmodValues (selModWhere mods) == [toField jobID] = do
                changeEmulator $ \_ -> next
                return 1
          mkFakeDelete jobID _ tbl mods = failEmulator $
            "Can't handle DELETE: " ++ show (jobID, tbl, mods)

          mkEmulator jobID next =
            selectEmulator { deDelete = mkFakeDelete jobID next }

          emulator = mkEmulator (JobID 3333) $ mkEmulator (JobID 222) $
            mkEmulator (JobID 11) mempty

          cap = MkCapability $ \d -> case d of
            CADeleteJob -> AccessGranted
            CAGetJob    -> AccessGranted
            _           -> AccessDenied

      test cap emulator (deleteJob $ JobID 11) `shouldBe` Right ()
