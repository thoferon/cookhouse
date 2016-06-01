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

generateJobs :: MonadDataLayer m => [Project] -> ProjectIdentifier
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
createJobsBasedOnOrder :: MonadDataLayer m => ([EntityID Job], [EntityID Job])
                       -> JobType -> [[Project]]
                       -> m ([EntityID Job], [EntityID Job])
createJobsBasedOnOrder acc _ [] = return acc
createJobsBasedOnOrder (allIDs, lastIDs) typ (projects : dependentProjects) = do
  newIDs <- forM projects $ \project -> do
    createJob typ (projectIdentifier project) lastIDs
  createJobsBasedOnOrder (allIDs ++ newIDs, newIDs) typ dependentProjects

-- | Group the projects in group in which they can be built in parallel and
-- between which it should be sequential
--
-- Here's a quick explanation. Take the following dependency tree:
--      A
--    / | \
--   B  C  D
--   |  |
--   E  G
--  / \
--  F  G
--
-- We want to be able to "see" it as:
--      A
--    / | \
--   B  |  |
--   |  |  |
--   E  C  |
--  / \ |  |
--  F G G  D
--
-- What this function does is to return a list like the following:
-- [[F,G,D], [E,C], [B], [A]]
--
-- Note that removing duplicate in the same group is enough as a project is
-- guaranteed to always appear at the same "level" in the modified graph.
computeBuildOrder :: [Project] -> ProjectIdentifier
                  -> Either CookhouseError [[Project]]
computeBuildOrder projects identifier = do
  project <- findProject projects identifier
  orders  <- mapM (computeBuildOrder projects) (projectDependencies project)
  let merged = mergeOrders orders
  return $ merged ++ [[project]]

mergeOrders :: [[[Project]]] -> [[Project]]
mergeOrders = foldl' (zipWithLargest (\xs ys -> nub $ xs ++ ys)) []
  where
    -- Like zipWith but it doesn't stop at the shortest list
    zipWithLargest :: (a -> a -> a) -> [a] -> [a] -> [a]
    zipWithLargest _ xs [] = xs
    zipWithLargest _ [] ys = ys
    zipWithLargest f (x:xs) (y:ys) = f x y : zipWithLargest f xs ys

findProject :: [Project] -> ProjectIdentifier -> Either CookhouseError Project
findProject projects identifier =
  case find ((==identifier) . projectIdentifier) projects of
    Nothing -> throwError $
      IncorrectProjectIdentifierError $ unProjectIdentifier identifier
    Just project -> return project
