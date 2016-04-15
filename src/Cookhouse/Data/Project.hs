module Cookhouse.Data.Project
  ( Project(..)
  , Source(..)
  , Trigger(..)
  , Step(..)
  , PluginConfig
  , SimpleValue(..)
  ) where

import           Control.Applicative
import           Control.Monad

import           Data.Aeson
import           Data.Maybe
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T

data Project = Project
  { projectIdentifier     :: String    -- ^ A unique string
  , projectSource         :: Source    -- ^ Location of the code (e.g. a repo)
  , projectDependencies   :: [String]  -- ^ Project identifiers of dependencies
  , projectTriggers       :: [Trigger] -- ^ Events on which to trigger a build
  , projectBuildSteps     :: [Step]    -- ^ Steps to build the project
  , projectPostBuildsteps :: [Step]    -- ^ Steps to perform afterwards
                                       -- (e.g. deployment)
  }

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
  { sourcePluginName :: String -- ^ Name of a plugin that can handle this source
  , sourceLocation   :: String -- ^ File path, URL, ...
  }

instance FromJSON Source where
  parseJSON = withObject "Source" $ \obj ->
    Source <$> obj .: "plugin" <*> obj .: "location"

instance ToJSON Source where
  toJSON (Source{..}) = object
     [ "plugin"   .= sourcePluginName
     , "location" .= sourceLocation
     ]

-- | Event that should trigger a build
data Trigger = Trigger
  { triggerPluginName :: String -- ^ Name of a plugin that can detect this event
  , triggerConfig     :: PluginConfig
                         -- ^ Extra values for plugin configuration
  }

instance FromJSON Trigger where
  parseJSON = withObject "Trigger" $ \obj -> do
    config <- fmap catMaybes . forM (M.toList obj) $ \(k,v) -> case k of
      "plugin" -> return Nothing
      _ -> do
        v' <- parseJSON v
        return $ Just (T.unpack k, v')
    Trigger <$> obj .: "plugin" <*> pure config

-- | Action to perform in order to build or deploy the project
data Step = Step
  { stepPluginName :: String       -- ^ Name of a plugin that will perform it
  , stepConfig     :: PluginConfig -- ^ Extra values for plugin configuration
  }

instance FromJSON Step where
  parseJSON = withObject "Step" $ \obj -> do
    config <- fmap catMaybes . forM (M.toList obj) $ \(k,v) -> case k of
      "plugin" -> return Nothing
      _ -> do
        v' <- parseJSON v
        return $ Just (T.unpack k, v')
    Step <$> obj .: "plugin" <*> pure config

type PluginConfig = [(String, SimpleValue)]

data SimpleValue
  = SVBool   Bool
  | SVInt    Int
  | SVString String

instance FromJSON SimpleValue where
  parseJSON v = do
    (SVBool <$> parseJSON v)
    <|> (SVInt <$> parseJSON v)
    <|> (SVString <$> parseJSON v)
