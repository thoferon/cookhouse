module Cookhouse.Plugins.Step.Haskell
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
  { stepPluginName     = "haskell"
  , stepPluginRun      = compileProject
  , stepPluginRollback = emptyRollback
  }

compileProject :: FilePath -> [(String, String)] -> Handle -> PluginConfig
               -> PluginM Bool
compileProject dir envVars handle config = do
  let test        = fromMaybe False $ lookupConfigBool config "test"
      systemGHC   = fromMaybe False $ lookupConfigBool config "system-ghc"
      mGHCOptions = lookupConfigString config "ghc-options"

      args = (if systemGHC then ["--system-ghc"] else [])
             ++ ["build"]
             ++ (if test then ["--test"] else [])
             ++ maybe [] (\opts -> ["--ghc-options", opts]) mGHCOptions

  code <- liftIO $
    waitForProcess =<< runProcess "stack" args (Just dir) (Just envVars) Nothing
                                  (Just handle) (Just handle)
  return $ code == ExitSuccess
