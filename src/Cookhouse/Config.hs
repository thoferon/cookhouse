{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cookhouse.Config
  ( Config(..)
  , readConfigOrDie
  , mkConnectionPool
  ) where

import           Data.Aeson (withObject)
import           Data.Maybe
import           Data.Pool
import           Data.Yaml
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import           System.Exit
import           System.IO

import           Database.Seakale.PostgreSQL

import           Network.URI hiding (path)

import           Cookhouse.Data.Project

data Config = Config
  { configDatabaseUsername :: String
  , configDatabasePassword :: String
  , configDatabaseHostname :: String
  , configDatabasePort     :: Integer
  , configDatabaseName     :: String
  , configPort             :: Int
  , configCORSOrigins      :: [URI]
  , configBuildDirectory   :: FilePath
  , configProjects         :: [Project]
  , configMaxJobCount      :: Int
  , configPlugins          :: [(String, PluginConfig)]
  }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj -> do
    pluginConfigs <- do
      mConfigs <- obj .:? "plugin-config"
      return $ case mConfigs of
        Nothing -> []
        Just cfgs -> flip map (HM.toList cfgs) $ \(name, cfg) ->
          ( T.unpack name
          , map (\(k,v) -> (T.unpack k, v)) (HM.toList cfg) )
    db <- obj .: "database"
    Config
      <$> db  .: "username"
      <*> db  .: "password"
      <*> db  .: "hostname"
      <*> db  .: "port"
      <*> db  .: "name"
      <*> obj .: "port"
      <*> (fromMaybe [] <$> obj .:? "cors-origins")
      <*> obj .: "build-directory"
      <*> obj .: "projects"
      <*> obj .: "max-job-count"
      <*> pure pluginConfigs

readConfigOrDie :: FilePath -> IO Config
readConfigOrDie path = do
  configJson <- BS.readFile path
  case decodeEither configJson of
    Right config -> case checkProjects (configProjects config) of
      Nothing  -> return config
      Just err -> do
        hPutStrLn stderr $ "Projects are not sound: " ++ err
        exitFailure
    Left err -> do
      hPutStrLn stderr $ "Can't read the config file: " ++ err
      exitFailure

createConn :: Config -> IO Connection
createConn config = connect ConnectInfo
  { ciHostname = configDatabaseHostname config
  , ciUsername = configDatabaseUsername config
  , ciPassword = configDatabasePassword config
  , ciDatabase = configDatabaseName     config
  , ciPort     = fromInteger $ configDatabasePort config
  }

destroyConn :: Connection -> IO ()
destroyConn = disconnect

mkConnectionPool :: Config -> IO (Pool Connection)
mkConnectionPool config =
  createPool (createConn config) destroyConn 3 (24*60*60) 1

instance FromJSON URI where
  parseJSON obj = do
    str <- parseJSON obj
    case parseURI str of
      Just uri -> return uri
      Nothing  -> fail "Invalid URI."
