{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module Cookhouse.Capabilities
  ( CookhouseAccess(..)
  , anonymousCapability
  , singleCapability
  , toCookhouseCapability
  , module Control.SafeAccess
  ) where

import Control.SafeAccess hiding (getCapabilities)

import Cookhouse.Plugins.Types

data CookhouseAccess
  = CAGetProjects
  | CACreateJob
  deriving (Eq, Show)

anonymousCapability :: Capability CookhouseAccess
anonymousCapability = MkCapability $ \d -> case d of
  _ -> AccessDeniedSoft

-- | This is intended to be used in the tests ONLY.
singleCapability :: CookhouseAccess -> Capability CookhouseAccess
singleCapability d = MkCapability $ \d' ->
  if d == d' then AccessGranted else AccessDenied

toCookhouseCapability :: AccessLevel -> Capability CookhouseAccess
toCookhouseCapability level = MkCapability $ \d -> case (level, d) of
  (Admin, _)          -> AccessGranted
  (User, CACreateJob) -> AccessGranted
  (_, CAGetProjects)  -> AccessGranted
  _ -> AccessDeniedSoft
