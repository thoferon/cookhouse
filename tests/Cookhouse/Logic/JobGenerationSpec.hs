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

fakeInsert :: ToRow p => Query -> [Query] -> Query -> [p] -> EmulatorM [PureRow]
fakeInsert "jobs" _ "id" vals
  | [toRow (Job Build JobInQueue "commons" [] Nothing someTime)]
    == map toRow vals = return [[integer 42]]
  | [toRow (Job PostBuild JobInQueue "commons" [JobID 42] Nothing someTime)]
    == map toRow vals = return [[integer 142]] -- for the single test
  | [toRow (Job Build JobInQueue "libA" [JobID 42] Nothing someTime)]
    == map toRow vals = return [[integer 76]]
  | [toRow (Job Build JobInQueue "libB" [JobID 42] Nothing someTime)]
    == map toRow vals = return [[integer 89]]
  | [toRow (Job Build JobInQueue "website" [JobID 76, JobID 89] Nothing someTime)]
    == map toRow vals = return [[integer 137]]
  | [toRow (Job PostBuild JobInQueue "commons" [JobID 137] Nothing someTime)]
    == map toRow vals = return [[integer 242]]
  | [toRow (Job PostBuild JobInQueue "libA" [JobID 242] Nothing someTime)]
    == map toRow vals = return [[integer 476]]
  | [toRow (Job PostBuild JobInQueue "libB" [JobID 242] Nothing someTime)]
    == map toRow vals = return [[integer 589]]
  | [toRow (Job PostBuild JobInQueue "website" [JobID 476, JobID 589] Nothing
                someTime)]
    == map toRow vals = return [[integer 1337]]
fakeInsert tbl cols ret vals = failEmulator $
  "Can't handle INSERT query: " ++ show (tbl, cols, ret, map toRow vals)

spec :: Spec
spec = do
  describe "generateJobs" $ do
    let cap      = singleCapability CACreateJob
        emulator = mempty { deInsert = fakeInsert }
        test'    = test cap emulator

    it "throws a CookhouseError if computeBuildOder fails" $ do
      test' (generateJobs projects "incorrect") `shouldSatisfy` isLeft

    it "creates two jobs in the DB for a project without dependencies" $ do
      let eIDs = test' (generateJobs projects "commons")
      eIDs `shouldSatisfy` isRight
      let Right ids = eIDs
      ids `shouldContain` [JobID 42, JobID 142]

    it "creates a list of dependent jobs for a project with dependencies" $ do
      let eIDs = test' (generateJobs projects "website")
      eIDs `shouldSatisfy` isRight
      let Right ids = eIDs
      ids `shouldContain` [ JobID 42, JobID 76, JobID 89, JobID 137, JobID 242
                          , JobID 476, JobID 589, JobID 1337 ]

  describe "computeBuildOrder" $ do
    it "returns a single project if the project has no dependencies" $ do
      computeBuildOrder [commonsProject] "commons"
        `shouldBe` Right [[commonsProject]]

    it "returns a list of lists of projects (sequential for the first\
       \ and parallel builds for the latters)" $ do
      let order = [ [commonsProject]
                  , [libAProject, libBProject]
                  , [websiteProject]
                  ]
      computeBuildOrder projects "website" `shouldBe` Right order

    it "optimises so that projects are built as early as possible" $ do
      -- independentProject is the first group even though
      -- it is a direct dependency of websiteProject
      let order = [ [commonsProject, independentProject]
                  , [libAProject, libBProject]
                  , [websiteWithIndependentProject]
                  ]
      computeBuildOrder projects "website-with-independent"
        `shouldBe` Right order

    it "returns an error if an incorrect project identifier is given" $ do
      computeBuildOrder projects "wrong" `shouldSatisfy` isLeft
