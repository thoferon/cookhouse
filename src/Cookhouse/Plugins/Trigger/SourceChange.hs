module Cookhouse.Plugins.Trigger.SourceChange
  ( plugin
  ) where

import Control.Monad.Except

import System.FilePath
import System.Directory

import Cookhouse.Plugins.Types

plugin :: TriggerPlugin
plugin = TriggerPlugin
  { triggerPluginName  = "source-change"
  , triggerPluginCheck = checkRepository
  }

checkRepository :: (FilePath -> PluginM ()) -> (FilePath -> PluginM Bool)
                -> FilePath -> PluginConfig -> PluginM Bool
checkRepository fetchSource checkSource projectDir _ = do
  let repoDir = projectDir </> "trigger-test-repo"
  checkExistence <- liftIO $ doesDirectoryExist repoDir
  if checkExistence
    then fetchSource repoDir >> return True
    else checkSource repoDir
