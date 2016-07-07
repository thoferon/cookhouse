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

import Data.Binary

import Cookhouse.Plugins.Types

data CookhouseAccess
  = CAGetProjects
  | CAGetJob
  | CACreateJob
  | CAEditJob
  | CADeleteJob
  | CAGetJobResult
  | CACreateJobResult
  deriving (Eq, Show, Generic)

instance Binary CookhouseAccess

anonymousCapability :: Capability CookhouseAccess
anonymousCapability = MkCapability $ \d -> case d of
  _ -> AccessDeniedSoft

toCookhouseCapability :: AccessLevel -> Capability CookhouseAccess
toCookhouseCapability level = MkCapability $ \d -> case (level, d) of
  (Admin, _)          -> AccessGranted
  (User, CACreateJob) -> AccessGranted
  (_, CAGetJob)       -> AccessGranted
  (_, CAGetJobResult) -> AccessGranted
  (_, CAGetProjects)  -> AccessGranted
  _ -> AccessDeniedSoft

triggerWorkerCapability :: Capability CookhouseAccess
triggerWorkerCapability = MkCapability $ \d -> case d of
  CACreateJob -> AccessGranted
  _ -> AccessDeniedSoft

jobWorkerCapability :: Capability CookhouseAccess
jobWorkerCapability = MkCapability $ \d -> case d of
  CAGetJob          -> AccessGranted
  CAEditJob         -> AccessGranted
  CACreateJobResult -> AccessGranted
  _ -> AccessDeniedSoft
