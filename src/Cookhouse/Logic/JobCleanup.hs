module Cookhouse.Logic.JobCleanup
  ( deleteOldJobs
  , removeOldJobDirectories
  ) where

import Control.Monad.Except

import Data.Function
import Data.List

import System.Directory

import Cookhouse.Data.Job
import Cookhouse.Data.Project
import Cookhouse.Data.Types
import Cookhouse.Environment
import Cookhouse.Errors

deleteOldJobs :: MonadDataLayer m s => ProjectIdentifier -> m [Entity Job]
deleteOldJobs identifier = do
  ents <- getJobsOfProject identifier
  let (newJobs, oldJobs) = filterJobsToDelete ents
  mapM_ (deleteJob . entityID) oldJobs
  return newJobs

filterJobsToDelete :: [Entity Job] -> ([Entity Job], [Entity Job])
filterJobsToDelete =
    go 24 5 [] . sortBy (flip compare `on` (jobCreationTime . entityVal))
  where
    go :: Int -> Int -> [Entity Job] -> [Entity Job]
       -> ([Entity Job], [Entity Job])
    go _ _ acc [] = (acc, [])
    go n m acc ents@(ent@(Entity _ Job{..}) : ents')
      | n <= 0 && m <= 0 = (acc, ents)
      | jobStatus == JobSuccess = go (n-1) (m-1) (ent : acc) ents'
      | otherwise = go (n-1) m (ent : acc) ents'

removeOldJobDirectories :: ( MonadIO m, HasEnvironment m
                           , MonadError CookhouseError m )
                        => ProjectIdentifier -> [Entity Job] -> m ()
removeOldJobDirectories identifier ents = do
  let legitimatePaths = map (jobDirectory . entityVal) ents
  projects <- getProjects
  project  <- getProject projects identifier
  dir      <- getProjectDirectory project
  paths    <- liftIO $ listDirectory dir
  liftIO $ mapM_ removeDirectoryRecursive $ paths \\ legitimatePaths
