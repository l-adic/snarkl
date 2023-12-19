module Control.Monad.Supply
  ( SupplyT (..),
    Supply,
    runSupplyT,
    runSupply,
  )
where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (StateT (runStateT))
import Control.Monad.Trans (MonadTrans)

newtype SupplyT m a = SupplyT {unSupplyT :: StateT Int m a}
  deriving (Functor, Applicative, Monad, MonadTrans)

runSupplyT :: SupplyT m a -> Int -> m (a, Int)
runSupplyT m = runStateT (unSupplyT m)

type Supply = SupplyT Identity

runSupply :: Supply a -> Int -> (a, Int)
runSupply m i = runIdentity $ runSupplyT m i
