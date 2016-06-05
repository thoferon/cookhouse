{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cookhouse.Data.Project
  ( ProjectIdentifier(..)
  , Project(..)
  , Source(..)
  , Trigger(..)
  , Step(..)
  , PluginConfig
  , SimpleValue(..)
  , checkProjects
  ) where

import           Control.Monad

import           Data.Aeson
import           Data.List
import           Data.Maybe
import           Data.String
import           Data.Time
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T

import           Web.PathPieces

import           Database.PostgreSQL.Simple.ToField

import           Cookhouse.Data.Types
import           Cookhouse.Plugins.Types hiding (triggerPluginName)

newtype ProjectIdentifier
  = ProjectIdentifier { unProjectIdentifier :: String }
  deriving (Eq, IsString, PureFromField, ToField, ToJSON, FromJSON, PathPiece)

instance Show ProjectIdentifier where
  show = unProjectIdentifier

data Project = Project
  { projectIdentifier     :: ProjectIdentifier -- ^ A unique string
  , projectSource         :: Source    -- ^ Location of the code (e.g. a repo)
  , projectDependencies   :: [ProjectIdentifier]
                                       -- ^ Project identifiers of dependencies
  , projectTriggers       :: [Trigger] -- ^ Events on which to trigger a build
  , projectBuildSteps     :: [Step]    -- ^ Steps to build the project
  , projectPostBuildsteps :: [Step]    -- ^ Steps to perform afterwards
                                       -- (e.g. deployment)
  } deriving (Eq, Show)

instance FromJSON Project where
  parseJSON = withObject "Project" $ \obj -> Project
    <$> obj .: "identifier"
    <*> obj .: "source"
    <*> (fromMaybe [] <$> obj .:? "dependencies")
    <*> (fromMaybe [] <$> obj .:? "triggers")
    <*> obj .: "build-steps"
    <*> (fromMaybe [] <$> obj .:? "post-build-steps")

instance ToJSON Project where
  toJSON (Project{..}) = object
    [ "identifier"       .= projectIdentifier
    , "source"           .= projectSource
    , "dependencies"     .= projectDependencies
    , "triggers"         .= map triggerPluginName projectTriggers
    , "build-steps"      .= map stepPluginName    projectBuildSteps
    , "post-build-steps" .= map stepPluginName    projectPostBuildsteps
    ]

-- | Source where to fetch the code from
data Source = Source
  { sourcePluginName :: String       -- ^ Name of a plugin to handle the source
  , sourceLocation   :: String       -- ^ File path, URL, ...
  , sourceConfig     :: PluginConfig -- ^ Extra values for plugin configuration
  } deriving (Eq, Show)

instance FromJSON Source where
  parseJSON = withObject "Source" $ \obj -> do
    config <- fmap catMaybes . forM (M.toList obj) $ \(k,v) ->
      if k `elem` ["plugin", "location"]
        then return Nothing
        else do
          v' <- parseJSON v
          return $ Just (T.unpack k, v')
    Source <$> obj .: "plugin" <*> obj .: "location" <*> pure config

instance ToJSON Source where
  toJSON (Source{..}) = object
     [ "plugin"   .= sourcePluginName
     , "location" .= sourceLocation
     ]

-- | Event that should trigger a build
data Trigger = Trigger
  { triggerPluginName :: String
    -- ^ Name of a plugin to check this event
  , triggerWaitingTime :: NominalDiffTime -- ^ Period of time between checks
  , triggerConfig      :: PluginConfig
    -- ^ Extra values for plugin configuration
  } deriving (Eq, Show)

instance FromJSON Trigger where
  parseJSON = withObject "Trigger" $ \obj -> do
    config <- fmap catMaybes . forM (M.toList obj) $ \(k,v) ->
      if k `elem` ["plugin", "waiting-time"]
        then return Nothing
        else do
          v' <- parseJSON v
          return $ Just (T.unpack k, v')
    Trigger <$> obj .: "plugin" <*> obj .: "waiting-time" <*> pure config

-- | Action to perform in order to build or deploy the project
data Step = Step
  { stepPluginName :: String       -- ^ Name of a plugin that will perform it
  , stepConfig     :: PluginConfig -- ^ Extra values for plugin configuration
  } deriving (Eq, Show)

instance FromJSON Step where
  parseJSON = withObject "Step" $ \obj -> do
    config <- fmap catMaybes . forM (M.toList obj) $ \(k,v) -> case k of
      "plugin" -> return Nothing
      _ -> do
        v' <- parseJSON v
        return $ Just (T.unpack k, v')
    Step <$> obj .: "plugin" <*> pure config

-- | Check whether the project are sound, i.e. dependencies exist and are not
 -- circular.
checkProjects :: [Project] -> Maybe String
checkProjects projects = either Just (const Nothing) $ do
    checkUniqueness  projects
    checkExistence   projects
    checkNonCircular [] projects

  where
    checkUniqueness :: [Project] -> Either String ()
    checkUniqueness [] = return ()
    checkUniqueness (p:ps)
      | all ((/= projectIdentifier p) . projectIdentifier) ps =
          checkUniqueness ps
      | otherwise =
          Left $ "Duplicate project identifier: " ++ show (projectIdentifier p)

    checkExistence :: [Project] -> Either String ()
    checkExistence [] = return ()
    checkExistence (p:ps) =
      case projectDependencies p
             `areDependenciesIn` map projectIdentifier projects of
        Nothing -> checkExistence ps
        Just missing -> Left $ "Missing dependency " ++ show missing ++ " for "
                               ++ show (projectIdentifier p)

    checkNonCircular :: [Project] -> [Project] -> Either String ()
    checkNonCircular _ [] = return ()
    checkNonCircular acc allProjects@(p:ps) = case map projectIdentifier ps of
      identifiers
        | length acc == length allProjects -> Left $
            "Circular dependency in the following projects: "
            ++ intercalate ", " (map (show . projectIdentifier) allProjects)
        | all (`elem` identifiers) (projectDependencies p)
          && all (all (/= projectIdentifier p) . projectDependencies) ps->
            checkNonCircular [] ps
        | otherwise -> checkNonCircular (p : acc) (ps ++ [p])

    areDependenciesIn :: [ProjectIdentifier] -> [ProjectIdentifier]
                      -> Maybe ProjectIdentifier
    areDependenciesIn [] _ = Nothing
    areDependenciesIn (pid:pids) identifiers
      | pid `elem` identifiers = areDependenciesIn pids identifiers
      | otherwise = Just pid
