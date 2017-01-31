{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module Cookhouse.Capabilities
  ( CookhouseAccess(..)
  , anonymousCapability
  , toCookhouseCapability
  , triggerWorkerCapability
  , jobWorkerCapability
  , module Control.SafeAccess
  ) where

import GHC.Generics

import Control.SafeAccess hiding (getCapabilities)

import Cookhouse.Plugins.Types

data CookhouseAccess
  = CAGetProjects
  | CAGetJob
  | CACreateJob
  | CAEditJob
  | CADeleteJob
  | CAGetJobResult
  | CACreateJobResult
  | CAEditJobResult
  deriving (Eq, Show, Generic)

anonymousCapability :: Monad m => Capability m CookhouseAccess
anonymousCapability = MkCapability $ \_ -> return AccessDeniedSoft

toCookhouseCapability :: Monad m => AccessLevel -> Capability m CookhouseAccess
toCookhouseCapability level = MkCapability $ \d -> return $ case (level, d) of
  (Admin, _)          -> AccessGranted
  (User, CACreateJob) -> AccessGranted
  (_, CAGetJob)       -> AccessGranted
  (_, CAGetJobResult) -> AccessGranted
  (_, CAGetProjects)  -> AccessGranted
  _ -> AccessDeniedSoft

triggerWorkerCapability :: Monad m => Capability m CookhouseAccess
triggerWorkerCapability = MkCapability $ \d -> return $ case d of
  CACreateJob -> AccessGranted
  _ -> AccessDeniedSoft

jobWorkerCapability :: Monad m => Capability m CookhouseAccess
jobWorkerCapability = MkCapability $ \d -> return $ case d of
  CAGetJob          -> AccessGranted
  CAEditJob         -> AccessGranted
  CADeleteJob       -> AccessGranted
  CAGetJobResult    -> AccessGranted
  CACreateJobResult -> AccessGranted
  CAEditJobResult   -> AccessGranted
  _ -> AccessDeniedSoft
