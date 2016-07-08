{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cookhouse.Workers.Helpers
  ( WorkerM
  , runWorker
  , getPool
  , inDataLayer
  , logError
  , getProjectDirectory
  , workerMain
  ) where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader

import Data.Pool
import Data.Time

import System.IO
import System.Exit

import Database.PostgreSQL.Simple

import Cookhouse.Capabilities
import Cookhouse.Config
import Cookhouse.Environment
import Cookhouse.Errors
import Cookhouse.Data.Types

type WorkerM
  = ReaderT (Environment, Capability CookhouseAccess, Pool Connection)
            (ExceptT CookhouseError IO)

instance HasEnvironment WorkerM where
  getEnvironment = (\(e,_,_) -> e) <$> ask

getCapability :: WorkerM (Capability CookhouseAccess)
getCapability = (\(_,c,_) -> c) <$> ask

getPool :: WorkerM (Pool Connection)
getPool = (\(_,_,p) -> p) <$> ask

withConnection :: (Connection -> WorkerM a) -> WorkerM a
withConnection f = do
  pool <- getPool
  (conn, localPool) <- liftIO $ takeResource pool
  let action  = f conn
      action' = catchError (Right <$> action) (return . Left)
  eRes <- catch (Right <$> action') (return . Left)
  liftIO $ putResource localPool conn

  case eRes of
    Left  err         -> throwError $ IOError $ show (err :: SomeException)
    Right (Left  err) -> throwError err
    Right (Right res) -> return res

inDataLayer :: DataM a -> WorkerM a
inDataLayer action = do
  now  <- liftIO getCurrentTime
  cap  <- getCapability
  eRes <- withConnection $ \conn -> liftIO $
    runDatabaseM conn $ runTimeT now $ runExceptT $ runSafeAccessT action [cap]
  case eRes of
    Left  err                   -> throwError err
    Right (Left  err)           -> throwError err
    Right (Right (Left  descr)) -> throwError $ PermissionError descr
    Right (Right (Right res))   -> return res

runWorker :: Environment -> Capability CookhouseAccess -> Pool Connection
          -> WorkerM a -> IO (Either CookhouseError a)
runWorker env cap pool action = runExceptT $ runReaderT action (env, cap, pool)

logError :: MonadIO m => CookhouseError -> m ()
logError = liftIO . hPutStrLn stderr . show

workerMain :: Capability CookhouseAccess -> WorkerM () -> Environment -> IO ()
workerMain cap action env = do
  pool <- mkConnectionPool $ envConfig env
  eRes <- runWorker env cap pool action
  case eRes of
    Left  err -> logError err
    Right ()  -> return ()
  exitFailure -- not supposed to terminate