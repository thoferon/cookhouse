module Cookhouse.Data.JobResultSpec where

import           Control.Monad

import           Data.List
import qualified Data.ByteString.Char8 as BS

import           Database.PostgreSQL.Simple.Types

import           Cookhouse.Capabilities
import           Cookhouse.Data.Job
import           Cookhouse.Data.JobResult

import           SpecHelpers

spec :: Spec
spec = do
  describe "jobPhaseToString/stringToJobPhase" $ do
    it "is (kind of) isomorphic" $ do
      forM_ [Run, Rollback] $ \p ->
        stringToJobPhase (jobPhaseToString p) `shouldBe` Just p

  let jobResultFields = [ integer 1337, integer 42, nullValue "varchar"
                        , PureField "job_phase" (Just "run")
                        , timestamp' someTime, nullValue "timestamp" ]
      jobResult = JobResult (JobID 42) Nothing Run someTime Nothing

  describe "getJobResult" $ do
    let fakeSelect "job_results" _ mods
          | wmodTemplate (selModWhere mods) == "id = ?"
            && wmodValues (selModWhere mods) == [toField (1337 :: Int)] =
              return [jobResultFields]
        fakeSelect tbl cols mods = failEmulator $
          "Can't handle SELECT: " ++ show (tbl, cols, mods)

        emulator = mempty { deSelect = fakeSelect }

    it "requires the capability CAGetJobResult" $ do
      test anonymousCapability mempty (getJobResult $ JobResultID 1337)
        `shouldBe` Left (AccessError CAGetJobResult)

    it "returns the job results for the given job ID" $ do
      test (singleCapability CAGetJobResult) emulator
           (getJobResult $ JobResultID 1337)
        `shouldBe` Right jobResult

  describe "getJobResultsFor" $ do
    let fakeSelect "job_results" _ mods
          | "job_id = ?" `isPrefixOf`
              BS.unpack (fromQuery (wmodTemplate (selModWhere mods)))
            && toField (42 :: Int) `elem` wmodValues (selModWhere mods) =
              return [jobResultFields]
        fakeSelect tbl cols mods = failEmulator $
          "Can't handle SELECT: " ++ show (tbl, cols, mods)

        emulator = mempty { deSelect = fakeSelect }

    it "requires the capability CAGetJobResult" $ do
      test anonymousCapability mempty (getJobResultsFor $ JobID 1)
        `shouldBe` Left (AccessError CAGetJobResult)

    it "returns the job results for the given job ID" $ do
      test (singleCapability CAGetJobResult) emulator
           (getJobResultsFor $ JobID 42)
        `shouldBe` Right [Entity (JobResultID 1337) jobResult]

  describe "createJobResult" $ do
    let fakeInsert "job_results" _ "id" vals
          | [toRow jobResult] == map toRow vals = return [[integer 9999]]
        fakeInsert tbl cols ret vals = failEmulator $
          "Can't handle INSERT: " ++ show (tbl, cols, ret, map toRow vals)

        emulator = mempty { deInsert = fakeInsert }

    it "requires the capability CACreateJobResult" $ do
      test anonymousCapability mempty (createJobResult (JobID 42) Run)
        `shouldBe` Left (AccessError CACreateJobResult)

    it "inserts a new job result in the DB" $ do
      test (singleCapability CACreateJobResult) emulator
           (createJobResult (JobID 42) Run)
        `shouldBe` Right (JobResultID 9999)

  describe "markJobResultOver" $ do
    let fakeUpdate "job_results" [("end_time", action), ("error", action')] mods
          | action == toField someTime && action' == toField ("Error" :: String)
            && wmodTemplate (selModWhere mods) == "id = ?"
            && wmodValues (selModWhere mods) == [toField (1337 :: Int)] =
              return 1
        fakeUpdate tbl pairs mods = failEmulator $
          "Can't handle UPDATE: " ++ show (tbl, pairs, mods)

        emulator = mempty { deUpdate = fakeUpdate }

    it "requires the capability CAEditJobResult" $ do
      test anonymousCapability mempty
           (markJobResultOver (JobResultID 1337) Nothing)
        `shouldBe` Left (AccessError CAEditJobResult)

    it "updates the error and sets the end time to now" $ do
      test (singleCapability CAEditJobResult) emulator
           (markJobResultOver (JobResultID 1337) (Just "Error"))
        `shouldBe` Right ()
