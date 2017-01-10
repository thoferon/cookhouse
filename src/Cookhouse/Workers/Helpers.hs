{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cookhouse.Workers.Helpers
  ( WorkerM
  , runWorker
  , inDataLayer
  , logError
  , getProjectDirectory
  , workerMain
  ) where

import Control.Monad.Except
import Control.Monad.Reader

import Data.Time

import System.IO
import System.Exit

import Cookhouse.Capabilities
import Cookhouse.Environment
import Cookhouse.Errors
import Cookhouse.Data.Types

type WorkerM
  = ReaderT (Environment, CookhouseCapability) (ExceptT CookhouseError IO)

instance HasEnvironment WorkerM where
  getEnvironment = (\(e,_) -> e) <$> ask

getCapability :: WorkerM CookhouseCapability
getCapability = (\(_,c) -> c) <$> ask

inDataLayer :: DataM a -> WorkerM a
inDataLayer action = do
  now  <- liftIO getCurrentTime
  cap  <- getCapability
  eRes <- runRequest $ runStore $ runTimeT now $ runExceptT $
    runSafeAccessT action [cap]
  case eRes of
    Left  err                   -> throwError $ SQLError err
    Right (Left  err)           -> throwError err
    Right (Right (Left  descr)) -> throwError $ PermissionError descr
    Right (Right (Right res))   -> return res

runWorker :: Environment -> CookhouseCapability -> WorkerM a
          -> IO (Either CookhouseError a)
runWorker env cap action = runExceptT $ runReaderT action (env, cap)

logError :: MonadIO m => CookhouseError -> m ()
logError = liftIO . hPutStrLn stderr . show

workerMain :: CookhouseCapability -> WorkerM () -> Environment -> IO ()
workerMain cap action env = do
  eRes <- runWorker env cap action
  case eRes of
    Left  err -> logError err
    Right ()  -> return ()
  exitFailure -- not supposed to terminate
