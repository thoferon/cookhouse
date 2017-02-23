module Cookhouse.Options
  ( Options(..)
  , parseOptions
  ) where

import Data.Monoid

import Options.Applicative
import Options.Applicative.Types

data Options = Options
  { optUser          :: Maybe String
  , optGroup         :: Maybe String
  , optConfigFile    :: String
  , optDaemon        :: Bool
  , optLogSQLQueries :: Bool
  }

parseOptions :: IO Options
parseOptions = execParser $ info (helper <*> parser) $
    fullDesc <> progDesc "Build system with web server and workers."
  where
    parser = Options
      <$> option (Just <$> readerAsk) (short 'u' <> long "user" <> value Nothing
                                       <> help "User to run the server as")
      <*> option (Just <$> readerAsk) (short 'g' <> long "group"
                                       <> value Nothing
                                       <> help "Group to run the server as")
      <*> option readerAsk (short 'c' <> long "config" <> value "config.yml"
                            <> help "Path to the config file")
      <*> flag False True (short 'd' <> long "daemon" <> help "Run as a daemon")
      <*> flag False True (short 'l' <> long "log-queries"
                           <> help "Log SQL queries to stderr")
