module Cookhouse.Plugins.Step.Mud
  ( plugin
  ) where

import Control.Monad.Trans

import Data.Maybe

import System.Exit
import System.IO
import System.Process

import Cookhouse.Plugins.Types

plugin :: StepPlugin
plugin = StepPlugin
  { stepPluginName     = "mud"
  , stepPluginRun      = callMud "deploy"   True
  , stepPluginRollback = callMud "rollback" False
  }

callMud :: String -> Bool -> FilePath -> [(String, String)] -> Handle
        -> PluginConfig -> PluginM Bool
callMud command withDir dir envVars handle config = do
  project <- getConfigString config "project"

  let sudo = fromMaybe False $ lookupConfigBool config "use-sudo"

      prog = if sudo then "sudo" else "mud"
      args = (if sudo then ["mud"] else [])
             ++ [command, project]
             ++ (if withDir then [dir] else [])

  code <- liftIO $
    waitForProcess =<< runProcess prog args (Just dir) (Just envVars) Nothing
                                  (Just handle) (Just handle)
  return $ code == ExitSuccess
