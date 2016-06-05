{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cookhouse.Config
  ( Config(..)
  , readConfigOrDie
  , createConn
  , destroyConn
  , withConnection
  ) where

import           Control.Exception
import           Control.Monad.Trans

import           Data.Aeson (withObject)
import           Data.Yaml
import qualified Data.ByteString.Char8 as BS

import           System.Exit
import           System.IO

import           Database.PostgreSQL.Simple

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
  }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj -> do
    db <- obj .: "database"
    Config
      <$> db  .: "username"
      <*> db  .: "password"
      <*> db  .: "hostname"
      <*> db  .: "port"
      <*> db  .: "name"
      <*> obj .: "port"
      <*> obj .: "cors-origins"
      <*> obj .: "build-directory"
      <*> obj .: "projects"

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
  { connectHost     = configDatabaseHostname config
  , connectUser     = configDatabaseUsername config
  , connectPassword = configDatabasePassword config
  , connectDatabase = configDatabaseName     config
  , connectPort     = fromInteger $ configDatabasePort config
  }

destroyConn :: Connection -> IO ()
destroyConn = close

withConnection :: MonadIO m => Config -> (Connection -> IO a) -> m a
withConnection config f = liftIO $ do
  conn <- createConn config
  res <- catch (f conn) $ \e -> close conn >> throw (e :: SomeException)
  close conn
  return res

instance FromJSON URI where
  parseJSON obj = do
    str <- parseJSON obj
    case parseURI str of
      Just uri -> return uri
      Nothing  -> fail "Invalid URI."
