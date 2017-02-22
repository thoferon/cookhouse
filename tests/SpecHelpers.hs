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

import Control.Monad.Except
import Control.SafeAccess

import Data.Either
import Data.Maybe
import Data.Time

import Database.Seakale.PostgreSQL (PSQL)
import Database.Seakale.Tests.Store

import Cookhouse.Capabilities
import Cookhouse.Data.Types hiding ( Property, runSelect, runSelectT
                                   , runStore, runStoreT )
import Cookhouse.Errors

import Test.Hspec hiding (after)
import Test.Hspec.QuickCheck
import Test.QuickCheck

someTime :: UTCTime
someTime = UTCTime { utctDay = ModifiedJulianDay 58000, utctDayTime = 40000 }

test' :: UTCTime -> CookhouseCapability -> Mock (StoreMock PSQL) ()
      -> DataM a -> (Either CookhouseError a, Mock (StoreMock PSQL) ())
test' time cap mock action =
  let (eRes, mock') = runStore' defaultPSQL mock $ runTimeT time $ runExceptT $
        runSafeAccessT action [cap]
      eRes' = case eRes of
        Left  err                   -> Left $ SQLError err
        Right (Left  err)           -> Left err
        Right (Right (Left access)) -> Left $ PermissionError access
        Right (Right (Right res))   -> Right res
  in (eRes', mock')

test :: CookhouseCapability -> Mock (StoreMock PSQL) () -> DataM a
     -> IO (Either CookhouseError a)
test cap mock action = do
  let (eRes, mock') = test' someTime cap mock action
  when (isRight eRes) $
    ("mockConsumed", mockConsumed mock')
      `shouldBe` ("mockConsumed" :: String, True)
  return eRes

someCapabilities :: [CookhouseAccess] -> CookhouseCapability
someCapabilities ds = MkCapability $ \d -> return $
  if d `elem` ds then AccessGranted else AccessDeniedSoft

singleCapability :: CookhouseAccess -> CookhouseCapability
singleCapability = someCapabilities . pure
