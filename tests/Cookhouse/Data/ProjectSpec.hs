module Cookhouse.Data.ProjectSpec where

import Data.Maybe

import Cookhouse.Data.Project

import Factory
import SpecHelpers

spec :: Spec
spec = do
  describe "stringToTimeSpec" $ do
    it "reads * *" $ do
      stringToTimeSpec "* *" `shouldBe` Just (TimeSpec TSPartAny TSPartAny)

    it "reads */5 *" $ do
      stringToTimeSpec "*/5 *"
        `shouldBe` Just (TimeSpec (TSPartMultiple 5) TSPartAny)

    it "reads 1,8,35 */2" $ do
      stringToTimeSpec "1,8,35 */2"
        `shouldBe` Just (TimeSpec (TSPartList [1,8,35]) (TSPartMultiple 2))

    it "reads 7 8,10,12" $ do
      stringToTimeSpec "7 8,10,12"
        `shouldBe` Just (TimeSpec (TSPartList [7]) (TSPartList [8,10,12]))

    it "fails with */63 *" $ do
      stringToTimeSpec "*/63 *" `shouldBe` Nothing

    it "fails with * * *" $ do
      stringToTimeSpec "* * *" `shouldBe` Nothing

  describe "doesTimeSpecMatch" $ do
    it "returns True for * *" $ property $ \(h,m) ->
      doesTimeSpecMatch (TimeSpec TSPartAny TSPartAny) h m `shouldBe` True

    it "returns True for an exact match" $ property $ \(h,m) ->
      doesTimeSpecMatch (TimeSpec (TSPartList [m]) (TSPartList [h])) h m
        `shouldBe` True

    it "returns True when the numbers are in the list" $ do
      doesTimeSpecMatch (TimeSpec (TSPartList [3,4,5]) (TSPartList [1,2,3])) 2 5
        `shouldBe` True

    it "returns True for multiples" $ do
      doesTimeSpecMatch (TimeSpec (TSPartMultiple 3) (TSPartMultiple 5)) 20 9
        `shouldBe` True

    it "returns False otherwise" $ do
      let wrongParts = [(TSPartMultiple 5, 6), (TSPartList [3,4,5], 9)]
          wrongSpecs =
            [ doesTimeSpecMatch (TimeSpec minPart hourPart) wrongHour wrongMin
            | (minPart,  wrongMin)  <- wrongParts
            , (hourPart, wrongHour) <- wrongParts ]
      mapM_ (`shouldBe` False) wrongSpecs

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
