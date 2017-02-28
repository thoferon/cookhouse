module Cookhouse.Plugins.Step.Make
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
  { stepPluginName     = "make"
  , stepPluginRun      = runMake
  , stepPluginRollback = emptyRollback
  }

runMake :: FilePath -> [(String, String)] -> Handle -> PluginConfig
        -> PluginM Bool
runMake dir envVars handle config = do
  let prog = fromMaybe "make" $ lookupConfigString config "executable"
      args = maybe [] pure    $ lookupConfigString config "target"

  code <- liftIO $
    waitForProcess =<< runProcess prog args (Just dir) (Just envVars) Nothing
                                  (Just handle) (Just handle)
  return $ code == ExitSuccess
