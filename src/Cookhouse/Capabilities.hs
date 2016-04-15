{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module Cookhouse.Capabilities
  ( CookhouseAccess(..)
  , anonymousCapability
  , toCookhouseCapability
  , module Control.SafeAccess
  ) where

import Control.SafeAccess hiding (getCapabilities)

import Cookhouse.Plugins.Types

data CookhouseAccess
  = CAGetProjects
  deriving Show

anonymousCapability :: Capability CookhouseAccess
anonymousCapability = MkCapability $ \d -> case d of
  _ -> AccessDeniedSoft

toCookhouseCapability :: AccessLevel -> Capability CookhouseAccess
toCookhouseCapability level = MkCapability $ \d -> case (level, d) of
  (Admin, _)         -> AccessGranted
  (_, CAGetProjects) -> AccessGranted
  _ -> AccessDeniedSoft
