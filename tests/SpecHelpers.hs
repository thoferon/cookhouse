{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module SpecHelpers
  ( module SpecHelpers
  , module Data.Either
  , module Data.Maybe
  , module Test.Hspec
  , module Test.Hspec.QuickCheck
  , module Test.QuickCheck
  , module Database.Seakale.Tests.Store
  , module Cookhouse.Capabilities
  , module Cookhouse.Data.Types
  , module Cookhouse.Errors
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Free
import           Control.Monad.State
import           Control.Monad.Trans.Free
import           Control.SafeAccess

import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import qualified Data.ByteString.Char8 as BS

import           Database.Seakale.PostgreSQL (PSQL)
import           Database.Seakale.Tests.Store

import           Cookhouse.Capabilities
import           Cookhouse.Data.Types hiding ( Property, runSelect, runSelectT
                                             , runStore, runStoreT )
import           Cookhouse.Errors

import           Test.Hspec hiding (after)
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

someTime :: UTCTime
someTime = UTCTime { utctDay = ModifiedJulianDay 58000, utctDayTime = 40000 }

testWithTime :: UTCTime -> CookhouseCapability -> Mock (StoreMock PSQL) ()
             -> DataM a -> Either CookhouseError a
testWithTime time cap mock action =
  let eRes = runStore PSQL mock $ runTimeT time $ runExceptT $
        runSafeAccessT action [cap]
  in case eRes of
       Left  err                   -> Left $ SQLError err
       Right (Left  err)           -> Left err
       Right (Right (Left access)) -> Left $ PermissionError access
       Right (Right (Right res))   -> Right res

test :: CookhouseCapability -> Mock (StoreMock PSQL) () -> DataM a
     -> Either CookhouseError a
test = testWithTime someTime

someCapabilities :: [CookhouseAccess] -> CookhouseCapability
someCapabilities ds = MkCapability $ \d -> return $
  if d `elem` ds then AccessGranted else AccessDeniedSoft

singleCapability :: CookhouseAccess -> CookhouseCapability
singleCapability = someCapabilities . pure
