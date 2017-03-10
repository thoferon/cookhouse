module Cookhouse.Actions.ArtefactActions where

import           Control.Monad.Trans

import           Data.List
import qualified Data.ByteString.Lazy as BSL

import           System.Directory
import           System.FilePath

import           Cookhouse.Actions.Types
import           Cookhouse.Data.Job
import           Cookhouse.Data.Project
import           Cookhouse.Data.Types
import           Cookhouse.Environment
import           Cookhouse.Errors

getArtefactOfProjectAction :: ProjectIdentifier -> Action String
getArtefactOfProjectAction identifier = do
  projects     <- getProjects
  project      <- getProject projects identifier
  Entity _ job <- getLatestSucceededJob identifier
  getArtefactAction project job

getArtefactOfJobAction :: EntityID Job -> Action String
getArtefactOfJobAction jobID = do
  job      <- getJob jobID
  projects <- getProjects
  project  <- getProject projects $ jobProjectIdentifier job
  getArtefactAction project job

getArtefactAction :: Project -> Job -> Action String
getArtefactAction project job = do
  dir <- getJobDirectory project job
  addArtefactDirectory dir

readArtefactAction :: String -> [FilePath] -> Action BSL.ByteString
readArtefactAction token segments = do
  mDir <- getArtefactDirectory token
  case mDir of
    Nothing  -> throwError $ ArtefactNotFound token
    Just dir -> do
      let path = foldl' (</>) dir segments
      path' <- liftIO $ canonicalizePath path
      check <- liftIO $ doesFileExist path'

      if dir `isPrefixOf` path' && check
        then liftIO $ BSL.readFile path'
        else throwError $ ArtefactNotFound path
