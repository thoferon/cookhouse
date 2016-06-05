module Cookhouse.Plugins.Source.Darcs
  ( plugin
  ) where

import Control.Monad.Except

import Data.List

import System.Exit
import System.Process

import Cookhouse.Plugins.Types

plugin :: SourcePlugin
plugin = SourcePlugin
  { sourcePluginName  = "darcs"
  , sourcePluginFetch = fetchRepository
  , sourcePluginPull  = pullRepository
  }

fetchRepository :: String -> FilePath -> PluginConfig -> PluginM ()
fetchRepository location dest _ = do
  (code, _, err) <- liftIO $
    readProcessWithExitCode "darcs" ["get", location, dest, "--lazy"] ""
  case code of
    ExitSuccess   -> return ()
    ExitFailure n -> throwError $
      "Fetching darcs repository at " ++ location ++ " failed with exit code "
      ++ show n ++ " and the following output on stderr: " ++ show err

pullRepository :: String -> FilePath -> PluginConfig -> PluginM Bool
pullRepository location dest _ = do
  (code, out, err) <- liftIO $
    readProcessWithExitCode "darcs" ["pull", "--repodir", dest, "--all"] ""
  case code of
    ExitSuccess   -> return ()
    ExitFailure n -> throwError $
      "Pulling darcs repository at " ++ location ++ " failed with exit code "
      ++ show n ++ " and the following output on stderr: " ++ show err
  return $ "Finished pulling" `isInfixOf` out
