module Cookhouse.Logic.JobResultOutputSpec where

import Cookhouse.Data.Job
import Cookhouse.Data.JobResult
import Cookhouse.Logic.JobResultOutput

import SpecHelpers

spec :: Spec
spec = do
  describe "jobOutputFile" $ do
    it "is different depending on the phase and job type" $ do
      jobOutputFile Run Build     `shouldNotBe` jobOutputFile Run PostBuild
      jobOutputFile Run Build     `shouldNotBe` jobOutputFile Rollback Build
      jobOutputFile Run PostBuild `shouldNotBe` jobOutputFile Rollback Build
