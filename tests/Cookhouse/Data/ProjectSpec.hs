module Cookhouse.Data.ProjectSpec where

import Data.Maybe

import Cookhouse.Data.Project

import Factory
import SpecHelpers

spec :: Spec
spec = do
  describe "checkProjects" $ do
    it "returns an error if a project has missing dependency" $ do
      checkProjects [websiteProject] `shouldSatisfy` isJust

    it "returns an error if identifiers are not unique" $ do
      checkProjects [commonsProject, commonsProject] `shouldSatisfy` isJust

    it "returns an error if there is a circular dependency" $ do
      let commonsProject' = commonsProject { projectDependencies = ["libA"] }
      checkProjects [commonsProject', libAProject] `shouldSatisfy` isJust

    it "returns Nothing if it is sound" $ do
      checkProjects [commonsProject, libAProject, libBProject, websiteProject]
        `shouldBe` Nothing
