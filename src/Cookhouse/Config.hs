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
  { configDatabaseUser     :: String
  , configDatabasePassword :: String
  , configDatabaseHost     :: String
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
      <$> db  .: "user"
      <*> db  .: "password"
      <*> db  .: "host"
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
    Right config -> return config
    Left err -> do
      hPutStrLn stderr $ "Can't read the config file: " ++ err
      exitFailure

createConn :: Config -> IO Connection
createConn config = connect ConnectInfo
  { connectHost     = configDatabaseHost     config
  , connectUser     = configDatabaseUser     config
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
