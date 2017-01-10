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
        `shouldBe` Left (PermissionError CAGetJobResult)

    it "returns the job results for the given job ID" $ do
      let mock = mockGet jobResultID jobResult
      test (singleCapability CAGetJobResult) mock
           (getJobResult $ JobResultID 1337)
        `shouldBe` Right jobResult

  describe "getJobResultsFor" $ do
    it "requires the capability CAGetJobResult" $ do
      test anonymousCapability mempty (getJobResultsFor $ JobID 1)
        `shouldBe` Left (PermissionError CAGetJobResult)

    it "returns the job results for the given job ID" $ do
      let mock = mockSelect (JobResultJobID ==. JobID 42)
                            (asc JobResultStartTime)
                            [Entity jobResultID jobResult]
      test (singleCapability CAGetJobResult) mock
           (getJobResultsFor $ JobID 42)
        `shouldBe` Right [Entity jobResultID jobResult]

  describe "createJobResult" $ do
    it "requires the capability CACreateJobResult" $ do
      test anonymousCapability mempty (createJobResult (JobID 42) Run)
        `shouldBe` Left (PermissionError CACreateJobResult)

    it "inserts a new job result in the DB" $ do
      let jr = JobResult (JobID 42) Nothing Run someTime Nothing
          mock = mockInsert jr (JobResultID 9999)
      test (singleCapability CACreateJobResult) mock
           (createJobResult (JobID 42) Run)
        `shouldBe` Right (JobResultID 9999)

  describe "markJobResultOver" $ do
    it "requires the capability CAEditJobResult" $ do
      test anonymousCapability mempty
           (markJobResultOver (JobResultID 1337) Nothing)
        `shouldBe` Left (PermissionError CAEditJobResult)

    it "updates the error and sets the end time to now" $ do
      let mock = mockUpdate jobResultID $
            JobResultEndTime =. Just someTime <> JobResultError =. Just "Error"
      test (singleCapability CAEditJobResult) mock
           (markJobResultOver jobResultID (Just "Error"))
        `shouldBe` Right ()
