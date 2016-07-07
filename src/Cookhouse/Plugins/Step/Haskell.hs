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

compileProject :: FilePath -> Handle -> PluginConfig -> PluginM Bool
compileProject dir handle config = do
  let test        = fromMaybe False $ lookupConfigBool config "test"
      mGHCOptions = lookupConfigString config "ghc-options"

      args = ["build", "--copy-bins"]
             ++ if test then ["--test"] else []
             ++ maybe [] (\opts -> ["--ghc-options", opts]) mGHCOptions

  code <- liftIO $
    waitForProcess =<< runProcess "stack" args (Just dir) Nothing Nothing
                                  (Just handle) (Just handle)
  return $ code == ExitSuccess
