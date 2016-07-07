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

  describe "jobOutputFile" $ do
    it "is different depending on the phase and job type" $ do
      let f p t = jobOutputFile p (Job t JobSuccess "identifier" [] someTime)
      f Run Build     `shouldNotBe` f Run PostBuild
      f Run Build     `shouldNotBe` f Rollback Build
      f Run PostBuild `shouldNotBe` f Rollback Build

  describe "getJobResultsFor" $ do
    let jobResultFields = [ integer 1337, integer 42, nullValue "varchar"
                          , PureField "job_phase" (Just "run")
                          , timestamp' someTime ]

        fakeSelect "job_results" _ mods
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
      let jobResult = JobResult (JobID 42) Nothing Run someTime
      test (singleCapability CAGetJobResult) emulator
           (getJobResultsFor $ JobID 42)
        `shouldBe` Right [Entity (JobResultID 1337) jobResult]

  describe "createJobResult" $ do
    let jobResult = JobResult (JobID 1) Nothing Run someTime

        fakeInsert "job_results" _ "id" vals
          | [toRow jobResult] == map toRow vals = return [[integer 9999]]
        fakeInsert tbl cols ret vals = failEmulator $
          "Can't handle INSERT: " ++ show (tbl, cols, ret, map toRow vals)

        emulator = mempty { deInsert = fakeInsert }

    it "requires the capability CACreateJobResult" $ do
      test anonymousCapability mempty (createJobResult (JobID 1) Nothing Run)
        `shouldBe` Left (AccessError CACreateJobResult)

    it "inserts a new job result in the DB" $ do
      test (singleCapability CACreateJobResult) emulator
           (createJobResult (JobID 1) Nothing Run)
        `shouldBe` Right (JobResultID 9999)
