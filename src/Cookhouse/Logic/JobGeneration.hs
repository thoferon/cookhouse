module Cookhouse.Logic.JobGeneration
  ( generateJobs
  , computeBuildOrder
  ) where

import Control.Monad

import Data.List

import Cookhouse.Data.Job
import Cookhouse.Data.Project
import Cookhouse.Data.Types
import Cookhouse.Errors

generateJobs :: MonadDataLayer m s => [Project] -> ProjectIdentifier
             -> m [EntityID Job]
generateJobs projects identifier = do
  order <- case computeBuildOrder projects identifier of
    Left  err -> throwError err
    Right res -> return res

  acc         <- createJobsBasedOnOrder ([], []) Build order
  (allIDs, _) <- createJobsBasedOnOrder acc PostBuild order

  return allIDs

-- | Create jobs in order and take care of dependencies
--
-- This function takes an accumulator of the form
-- (all IDs of jobs generated so far, IDs generated at the last step)
-- and returns a new accumulator of the same form.
createJobsBasedOnOrder :: MonadDataLayer m s => ([EntityID Job], [EntityID Job])
                       -> JobType -> [[Project]]
                       -> m ([EntityID Job], [EntityID Job])
createJobsBasedOnOrder acc _ [] = return acc
createJobsBasedOnOrder (allIDs, lastIDs) typ (projects : dependentProjects) = do
  newIDs <- forM projects $ \project -> do
    createJob typ (projectIdentifier project) lastIDs
  createJobsBasedOnOrder (allIDs ++ newIDs, newIDs) typ dependentProjects

-- | Group the projects in groups in which they can be built in parallel and
-- between which it should be sequential
computeBuildOrder :: [Project] -> ProjectIdentifier
                  -> Either CookhouseError [[Project]]
computeBuildOrder allProjects identifier = do
    project <- getProject allProjects identifier
    go [project]
  where
    go :: [Project] -> Either CookhouseError [[Project]]
    go [] = return []
    go projects = do
      let directRevDeps = nub $ concatMap (reverseDependencies allProjects
                                           . projectIdentifier) projects
      subGroups <- go directRevDeps
      let allRevDeps      = concat subGroups
          clearedProjects = projects \\ allRevDeps
      return $ clearedProjects : subGroups

-- | Return the list of projects which directly depend on the project with the
-- given identifier
reverseDependencies :: [Project] -> ProjectIdentifier -> [Project]
reverseDependencies projects identifier =
  filter ((identifier `elem`) . projectDependencies) projects
