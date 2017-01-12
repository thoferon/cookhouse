module Cookhouse.Logic.JobGenerationSpec where

import Cookhouse.Data.Job
import Cookhouse.Data.Project
import Cookhouse.Logic.JobGeneration

import Factory
import SpecHelpers

projects :: [Project]
projects =
  [ commonsProject, libAProject, libBProject
  , websiteProject, independentProject, websiteWithIndependentProject
  ]

spec :: Spec
spec = do
  describe "generateJobs" $ do
    let cap = singleCapability CACreateJob

    it "throws a CookhouseError if computeBuildOder fails" $ do
      eRes <- test cap mempty (generateJobs projects "incorrect")
      eRes `shouldSatisfy` isLeft

    it "creates two jobs for a project with no reverse dependencies" $ do
      let job42  = Job Build     JobInQueue "website" []         someTime
          job142 = Job PostBuild JobInQueue "website" [JobID 42] someTime
          mock = mockInsert job42 (JobID 42) >> mockInsert job142 (JobID 142)
      eIDs <- test cap mock (generateJobs projects "website")
      eIDs `shouldSatisfy` isRight
      let Right ids = eIDs
      ids `shouldContain` [JobID 42, JobID 142]

    it "creates a list of dependent jobs for a project with reverse\
       \ dependencies" $ do
      let mock = do
            mockInsert (Job Build JobInQueue "commons" [] someTime)
                       (JobID 1)
            mockInsert (Job Build JobInQueue "libA" [JobID 1] someTime)
                       (JobID 2)
            mockInsert (Job Build JobInQueue "libB" [JobID 1] someTime)
                       (JobID 3)
            mockInsert (Job Build JobInQueue "website" [JobID 2, JobID 3]
                            someTime) (JobID 4)
            mockInsert (Job PostBuild JobInQueue "commons" [JobID 4] someTime)
                       (JobID 5)
            mockInsert (Job PostBuild JobInQueue "libA" [JobID 5] someTime)
                       (JobID 6)
            mockInsert (Job PostBuild JobInQueue "libB" [JobID 5] someTime)
                       (JobID 7)
            mockInsert (Job PostBuild JobInQueue "website" [JobID 6, JobID 7]
                            someTime) (JobID 8)

          projects' = [commonsProject, libAProject, libBProject, websiteProject]

      eIDs <- test cap mock (generateJobs projects' "commons")
      eIDs `shouldSatisfy` isRight
      let Right ids = eIDs
      ids `shouldContain` [ JobID 1, JobID 2, JobID 3, JobID 4, JobID 5, JobID 6
                          , JobID 7, JobID 8 ]

  describe "computeBuildOrder" $ do
    it "returns a single project if a project has no reverse dependencies" $ do
      computeBuildOrder [websiteProject] "website"
        `shouldBe` Right [[websiteProject]]

    it "returns a list of lists of projects (sequential for the first\
       \ and parallel builds for the latters)" $ do
      let projects' = [commonsProject, libAProject, libBProject, websiteProject]
          order = [ [commonsProject]
                  , [libAProject, libBProject]
                  , [websiteProject]
                  ]
      computeBuildOrder projects' "commons" `shouldBe` Right order

    it "returns an error if an incorrect project identifier is given" $ do
      computeBuildOrder projects "wrong" `shouldSatisfy` isLeft
