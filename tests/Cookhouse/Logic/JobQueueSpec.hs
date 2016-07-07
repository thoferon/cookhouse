module Cookhouse.Logic.JobQueueSpec where

import Cookhouse.Data.Job
import Cookhouse.Data.JobResult
import Cookhouse.Logic.JobQueue

import SpecHelpers

spec :: Spec
spec = do
  describe "getNextJob" $ do
    let job1Fields = [ integer 1, PureField "job_type" (Just "build")
                     , PureField "job_status" (Just "success")
                     , varchar "identifier", emptyArray "int4"
                     , timestamp' someTime ]
        job2Fields = [ integer 2, PureField "job_type" (Just "build")
                     , PureField "job_status" (Just "success")
                     , varchar "identifier", array "int4" "{1}"
                     , timestamp' someTime ]
        job3Fields = [ integer 3, PureField "job_type" (Just "build")
                     , PureField "job_status" (Just "in-queue")
                     , varchar "identifier", emptyArray "int4"
                     , timestamp' someTime ]

        fakeSelect b "jobs" _ mods
          | toField JobFailure `elem` wmodValues (selModWhere mods) && b =
              return [job2Fields]
          | toField JobInQueue `elem` wmodValues (selModWhere mods) =
              return [job3Fields]
          | b = return [job1Fields]
          | otherwise = return []
        fakeSelect _ tbl cols mods = failEmulator $
          "Can't handle SELECT: " ++ show (tbl, cols, mods)

        emulator b = mempty { deSelect = fakeSelect b }

    it "returns Nothing if nothing is available" $ do
      let _emulator = mempty { deSelect = \_ _ _ -> return [] }
      test jobWorkerCapability _emulator getNextJob
        `shouldBe` Right Nothing

    it "returns a rollback job when needed" $ do
      let job = Job Build JobSuccess "identifier" [] someTime
      test jobWorkerCapability (emulator True) getNextJob
        `shouldBe` Right (Just (MetaJob Rollback (Entity (JobID 1) job)))

    it "returns a run job when available" $ do
      let job = Job Build JobInQueue "identifier" [] someTime
      test jobWorkerCapability (emulator False) getNextJob
        `shouldBe` Right (Just (MetaJob Run (Entity (JobID 3) job)))
