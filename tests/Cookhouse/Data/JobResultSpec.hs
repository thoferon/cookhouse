module Cookhouse.Data.JobResultSpec where

import Control.Monad

import Data.Monoid

import Cookhouse.Capabilities
import Cookhouse.Data.Job
import Cookhouse.Data.JobResult

import SpecHelpers

spec :: Spec
spec = do
  describe "jobPhaseToString/stringToJobPhase" $ do
    it "is (kind of) isomorphic" $ do
      forM_ [Run, Rollback] $ \p ->
        stringToJobPhase (jobPhaseToString p) `shouldBe` Just p

  let jobResultID = JobResultID 1337
      jobResult   = JobResult (JobID 42) Nothing Run someTime Nothing

  describe "getJobResult" $ do
    it "requires the capability CAGetJobResult" $ do
      test anonymousCapability mempty (getJobResult $ JobResultID 1337)
        `shouldReturn` Left (PermissionError CAGetJobResult)

    it "returns the job results for the given job ID" $ do
      let mock = mockGet jobResultID jobResult
      test (singleCapability CAGetJobResult) mock
           (getJobResult $ JobResultID 1337)
        `shouldReturn` Right jobResult

  describe "getJobResultsFor" $ do
    it "requires the capability CAGetJobResult" $ do
      test anonymousCapability mempty (getJobResultsFor $ JobID 1)
        `shouldReturn` Left (PermissionError CAGetJobResult)

    it "returns the job results for the given job ID" $ do
      let mock = mockSelect (JobResultJobID ==. JobID 42)
                            (asc JobResultStartTime)
                            [Entity jobResultID jobResult]
      test (singleCapability CAGetJobResult) mock
           (getJobResultsFor $ JobID 42)
        `shouldReturn` Right [Entity jobResultID jobResult]

  describe "createJobResult" $ do
    it "requires the capability CACreateJobResult" $ do
      test anonymousCapability mempty (createJobResult (JobID 42) Run)
        `shouldReturn` Left (PermissionError CACreateJobResult)

    it "inserts a new job result in the DB" $ do
      let jr = JobResult (JobID 42) Nothing Run someTime Nothing
          mock = mockInsert jr (JobResultID 9999)
      test (singleCapability CACreateJobResult) mock
           (createJobResult (JobID 42) Run)
        `shouldReturn` Right (JobResultID 9999)

  describe "upsertJobResult" $ do
    let jr = JobResult (JobID 42) Nothing Run someTime Nothing

    it "requires the capability CAGetJobResult" $ do
      test anonymousCapability mempty (upsertJobResult (JobID 42) Run)
        `shouldReturn` Left (PermissionError CAGetJobResult)

    context "when it doesn't exist" $ do
      let mock = do
            mockSelect (JobResultJobID ==. JobID 42 &&. JobResultPhase ==. Run)
                       (limit 1) []
            mockInsert jr (JobResultID 9999)

      it "requires the capability CACreateJobResult" $ do
        test (singleCapability CAGetJobResult) mock
             (upsertJobResult (JobID 42) Run)
          `shouldReturn` Left (PermissionError CACreateJobResult)

      it "inserts a new job result in the DB" $ do
        test (someCapabilities [CAGetJobResult, CACreateJobResult]) mock
             (upsertJobResult (JobID 42) Run)
          `shouldReturn` Right (JobResultID 9999)

    context "when it already exists" $ do
      let mock = do
            mockSelect (JobResultJobID ==. JobID 42 &&. JobResultPhase ==. Run)
                       (limit 1) [Entity (JobResultID 9999) jr]
            mockUpdate (JobResultID 9999) (JobResultStartTime =. someTime
                                           <> JobResultEndTime =. Nothing)

      it "requires the capability CAEditJobResult" $ do
        test (singleCapability CAGetJobResult) mock
             (upsertJobResult (JobID 42) Run)
          `shouldReturn` Left (PermissionError CAEditJobResult)

      it "edits its start time and end time" $ do
        test (someCapabilities [CAGetJobResult, CAEditJobResult]) mock
             (upsertJobResult (JobID 42) Run)
          `shouldReturn` Right (JobResultID 9999)

  describe "markJobResultOver" $ do
    it "requires the capability CAEditJobResult" $ do
      test anonymousCapability mempty
           (markJobResultOver (JobResultID 1337) Nothing)
        `shouldReturn` Left (PermissionError CAEditJobResult)

    it "updates the error and sets the end time to now" $ do
      let mock = mockUpdate jobResultID $
            JobResultEndTime =. Just someTime <> JobResultError =. Just "Error"
      test (singleCapability CAEditJobResult) mock
           (markJobResultOver jobResultID (Just "Error"))
        `shouldReturn` Right ()
