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
  , stepPluginRun      = runCommand "command"
  , stepPluginRollback = runCommand "rollback-command"
  }

runCommand :: String -> FilePath -> [(String, String)] -> Handle -> PluginConfig
           -> PluginM Bool
runCommand name dir envVars handle config =
  case lookupConfigString config name of
    Nothing  -> return True
    Just cmd -> do
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
