module Cookhouse.Plugins.Step.Command
  ( plugin
  ) where

import Control.Monad.Trans

import System.Exit
import System.IO
import System.Process hiding (runCommand)

import Cookhouse.Plugins.Types

plugin :: StepPlugin
plugin = StepPlugin
  { stepPluginName     = "command"
  , stepPluginRun      = runCommand
  , stepPluginRollback = emptyRollback
  }

runCommand :: FilePath -> [(String, String)] -> Handle -> PluginConfig
        -> PluginM Bool
runCommand dir envVars handle config = do
  cmd <- getConfigString config "command"

  let cp = (shell cmd)
             { cwd     = Just dir
             , env     = Just envVars
             , std_in  = NoStream
             , std_out = UseHandle handle
             , std_err = UseHandle handle
             }

  code <- liftIO $ do
    (_,_,_,ph) <- createProcess cp
    waitForProcess ph
  return $ code == ExitSuccess
