{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Cookhouse.Data.Types
  ( module Cookhouse.Data.Types
  , module Database.Seakale
  , module Database.Seakale.PostgreSQL
  , module Database.Seakale.PostgreSQL.FromRow
  , module Database.Seakale.PostgreSQL.ToRow
  , Generic
  , throwError
  ) where

import           GHC.Generics

import           Control.Monad.Except
import           Control.Monad.Trans.Free

import           Data.Aeson
import           Data.Time
import qualified Data.HashMap.Strict as HM

import           Database.Seakale hiding (runQuery)
import           Database.Seakale.PostgreSQL
import           Database.Seakale.PostgreSQL.FromRow
import           Database.Seakale.PostgreSQL.ToRow

import           Cookhouse.Capabilities
import           Cookhouse.Errors

type SubDataM = ExceptT CookhouseError (TimeT Store)
type DataM = SafeAccessT CookhouseAccess SubDataM

type CookhouseCapability = Capability SubDataM CookhouseAccess

type MonadDataLayer m s
  = ( MonadSafeAccess CookhouseAccess m s
    , MonadError CookhouseError m
    , MonadStore PSQL m
    , MonadTime m
    )

instance MonadSeakaleBase backend m
  => MonadSeakaleBase backend (SafeAccessT a m) where
  getBackend = lift getBackend
  throwSeakaleError = lift . throwSeakaleError
  catchSeakaleError f handler = SafeAccessT $ \caps ->
    catchSeakaleError (runSafeAccessT f caps)
                      (flip runSafeAccessT caps . handler)

instance (ToJSON a, ToJSON (EntityID a)) => ToJSON (Entity a) where
  toJSON (Entity i v) =
    let Object m = toJSON v
    in Object $ HM.insert "id" (toJSON i) m

newtype TimeF a = GetTime (UTCTime -> a)

instance Functor TimeF where
  fmap f (GetTime g) = GetTime (f . g)

type TimeT = FreeT TimeF

class Monad m => MonadTime m where
  getTime :: m UTCTime

instance Monad m => MonadTime (FreeT TimeF m) where
  getTime = liftF $ GetTime id

instance {-# OVERLAPPABLE #-} (MonadTime m, MonadTrans t, Monad (t m))
         => MonadTime (t m) where
  getTime = lift getTime

instance MonadSeakaleBase backend m
  => MonadSeakaleBase backend (TimeT m) where
  getBackend = lift getBackend
  throwSeakaleError = lift . throwSeakaleError
  catchSeakaleError f handler = do
    t <- getTime
    lift $ catchSeakaleError (runTimeT t f) (runTimeT t . handler)

runTimeT :: Monad m => UTCTime -> TimeT m a -> m a
runTimeT now = iterT interpreter
  where
    interpreter :: TimeF (m a) -> m a
    interpreter (GetTime f) = f now
