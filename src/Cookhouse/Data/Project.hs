{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cookhouse.Data.Project
  ( ProjectIdentifier(..)
  , Project(..)
  , Source(..)
  , TimeSpecPart(..)
  , TimeSpec(..)
  , stringToTimeSpec
  , doesTimeSpecMatch
  , Trigger(..)
  , Step(..)
  , PluginConfig
  , SimpleValue(..)
  , checkProjects
  , findProject
  , getProject
  ) where

import           Control.Monad
import           Control.Monad.Except

import           Data.Aeson
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.String
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T

import           Servant.API

import           Cookhouse.Data.Types
import           Cookhouse.Errors
import           Cookhouse.Plugins.Types

newtype ProjectIdentifier
  = ProjectIdentifier { unProjectIdentifier :: String }
  deriving (Eq, IsString, Generic, ToJSON, FromJSON, FromHttpApiData)

instance FromRow PSQL One ProjectIdentifier
instance ToRow   PSQL One ProjectIdentifier

instance Show ProjectIdentifier where
  show = unProjectIdentifier

data Project = Project
  { projectIdentifier     :: ProjectIdentifier -- ^ A unique string
  , projectSource         :: Source    -- ^ Location of the code (e.g. a repo)
  , projectDependencies   :: [ProjectIdentifier]
                                        -- ^ Project identifiers of dependencies
  , projectTriggers       :: [Trigger]  -- ^ Events on which to trigger a build
  , projectBuildSteps     :: [Step]     -- ^ Steps to build the project
  , projectPostBuildSteps :: [Step]     -- ^ Steps to perform afterwards
                                        -- (e.g. deployment)
  , projectArtefacts      :: [FilePath] -- ^ Paths to files such as a coverage
                                        -- report which get linked to in the
                                        -- front-end
  } deriving (Eq, Show)

instance FromJSON Project where
  parseJSON = withObject "Project" $ \obj -> Project
    <$> obj .: "identifier"
    <*> obj .: "source"
    <*> (fromMaybe [] <$> obj .:? "dependencies")
    <*> (fromMaybe [] <$> obj .:? "triggers")
    <*> obj .: "build-steps"
    <*> (fromMaybe [] <$> obj .:? "post-build-steps")
    <*> (fromMaybe [] <$> obj .:? "artefacts")

instance ToJSON Project where
  toJSON (Project{..}) = object
    [ "identifier"       .= projectIdentifier
    , "source"           .= projectSource
    , "dependencies"     .= projectDependencies
    , "triggers"         .= projectTriggers
    , "build-steps"      .= map stepPlugin projectBuildSteps
    , "post-build-steps" .= map stepPlugin projectPostBuildSteps
    , "artefacts"        .= projectArtefacts
    ]

-- | Source where to fetch the code from
data Source = Source
  { sourcePlugin   :: String       -- ^ Name of a plugin to handle the source
  , sourceLocation :: String       -- ^ File path, URL, ...
  , sourceConfig   :: PluginConfig -- ^ Extra values for plugin configuration
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
     [ "plugin"   .= sourcePlugin
     , "location" .= sourceLocation
     ]

data TimeSpecPart
  = TSPartMultiple Int
  | TSPartList [Int]
  | TSPartAny
  deriving (Eq, Show)

data TimeSpec = TimeSpec
  { tspecMinute :: TimeSpecPart
  , tspecHour   :: TimeSpecPart
  } deriving (Eq, Show)

stringToTimeSpec :: String -> Maybe TimeSpec
stringToTimeSpec fullStr = do
    let (minStr, hourStr) = fmap (drop 1) $ break (==' ') fullStr
    minPart  <- readPart 60 minStr
    hourPart <- readPart 24 hourStr
    return TimeSpec { tspecMinute = minPart , tspecHour = hourPart }

  where
    readPart :: Int -> String -> Maybe TimeSpecPart
    readPart upperBound str = case str of
      "*" -> return TSPartAny
      '*' : '/' : numStr -> TSPartMultiple <$> readNum upperBound numStr
      _ -> do
        let numStrs = splitOn "," str
        nums <- sequence $ map (readNum upperBound) numStrs
        return $ TSPartList nums

    readNum :: Int -> String -> Maybe Int
    readNum upperBound str = case reads str of
      (n,""):_ | 0 <= n && n < upperBound -> return n
      _ -> Nothing

timeSpecToString :: TimeSpec -> String
timeSpecToString (TimeSpec{..}) =
    showPart tspecMinute ++ " " ++ showPart tspecHour
  where
    showPart :: TimeSpecPart -> String
    showPart part = case part of
      TSPartMultiple n  -> "*/" ++ show n
      TSPartList     ns -> intercalate "," $ map show ns
      TSPartAny         -> "*"

doesTimeSpecMatch :: TimeSpec -> Int -> Int -> Bool
doesTimeSpecMatch (TimeSpec{..}) hour minute =
    check tspecHour hour && check tspecMinute minute
  where
    check :: TimeSpecPart -> Int -> Bool
    check part n = case part of
      TSPartMultiple m -> n `mod` m == 0
      TSPartList ns    -> n `elem` ns
      TSPartAny        -> True

instance FromJSON TimeSpec where
  parseJSON = withText "TimeSpec" $ \txt ->
    case stringToTimeSpec $ T.unpack txt of
      Nothing -> fail $ "invalid time spec: " ++ T.unpack txt
      Just ts -> return ts

instance ToJSON TimeSpec where
  toJSON = toJSON . timeSpecToString

-- | Event that should trigger a build
data Trigger = Trigger
  { triggerPlugin   :: String       -- ^ Name of a plugin to check this event
  , triggerTimeSpec :: TimeSpec     -- ^ When to run the trigger
  , triggerConfig   :: PluginConfig -- ^ Extra values for plugin configuration
  } deriving (Eq, Show)

instance FromJSON Trigger where
  parseJSON = withObject "Trigger" $ \obj -> do
    config <- fmap catMaybes . forM (M.toList obj) $ \(k,v) ->
      if k `elem` ["plugin", "time-spec"]
        then return Nothing
        else do
          v' <- parseJSON v
          return $ Just (T.unpack k, v')
    Trigger <$> obj .: "plugin" <*> obj .: "time-spec" <*> pure config

instance ToJSON Trigger where
  toJSON (Trigger{..}) = object
    [ "name"      .= triggerPlugin
    , "time_spec" .= triggerTimeSpec
    ]

-- | Action to perform in order to build or deploy the project
data Step = Step
  { stepPlugin  :: String             -- ^ Name of a plugin that will perform it
  , stepSubdir  :: Maybe FilePath     -- ^ Subdirectory in which the plugin runs
  , stepEnvVars :: [(String, String)] -- ^ Overwritten environment variables
  , stepConfig  :: PluginConfig       -- ^ Extra values for plugin configuration
  } deriving (Eq, Show)

instance FromJSON Step where
  parseJSON = withObject "Step" $ \obj -> do
    config <- fmap catMaybes . forM (M.toList obj) $ \(k,v) ->
      if k `elem` ["plugin", "subdirectory", "environment"]
        then return Nothing
        else do
          v' <- parseJSON v
          return $ Just (T.unpack k, v')

    mEnvObj <- obj .:? "environment"
    env <- case mEnvObj of
      Nothing -> return []
      Just envObj -> forM (M.toList envObj) $ \(k,v) -> (k,) <$> parseJSON v

    Step <$> obj .: "plugin" <*> obj .:? "subdirectory"
         <*> pure env <*> pure config

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

findProject :: [Project] -> ProjectIdentifier -> Maybe Project
findProject projects identifier =
  find ((== identifier) . projectIdentifier) projects

getProject :: MonadError CookhouseError m
           => [Project] -> ProjectIdentifier -> m Project
getProject projects identifier = do
  case findProject projects identifier of
    Nothing -> throwError $
      IncorrectProjectIdentifierError $ unProjectIdentifier identifier
    Just project -> return project
