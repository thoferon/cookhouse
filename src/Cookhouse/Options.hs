module Cookhouse.Options
  ( Options(..)
  , getOptions
  ) where

import Data.Monoid

import Options.Applicative
import Options.Applicative.Types

data Options = Options
  { optUser       :: Maybe String
  , optGroup      :: Maybe String
  , optConfigFile :: String
  , optDaemon     :: Bool
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> parser) $
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
