module Cookhouse.Plugins.Step.Ocaml
  ( plugin
  ) where

import           Control.Monad.Except

import           System.Exit
import           System.IO
import qualified System.Process as P

import           Cookhouse.Plugins.Types

plugin :: StepPlugin
plugin = StepPlugin
  { stepPluginName     = "ocaml"
  , stepPluginRun      = runOcaml
  , stepPluginRollback = emptyRollback
  }

runOcaml :: FilePath -> [(String, String)] -> Handle -> PluginConfig
         -> PluginM Bool
runOcaml dir envVars handle config = do
    pkg <- getConfigString config "package"

    eRes <- runExceptT $ do
      case lookupConfigString config "switch" of
        Nothing -> return ()
        Just switch -> runProcess "opam" ["switch", switch]

      runProcess "oasis" ["setup"]
      runProcess "oasis2opam" ["-y", "--local"]
      runProcess "opam" ["pin", "add", pkg, dir, "-y"]

    either (\_ -> return False) (\_ -> return True) eRes

  where
    runProcess :: String -> [String] -> ExceptT () PluginM ()
    runProcess prog args = do
      code <- liftIO $ P.waitForProcess =<<
        P.runProcess prog args (Just dir) (Just envVars) Nothing
                     (Just handle) (Just handle)
      unless (code == ExitSuccess) $ throwError ()
