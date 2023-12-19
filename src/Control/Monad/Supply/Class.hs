module Control.Monad.Supply.Class where

import Control.Monad.State (MonadTrans (lift), StateT, get, put)
import Control.Monad.Supply (SupplyT (..))

class (Monad m) => MonadSupply m where
  fresh :: m Int
  peek :: m Int

instance (Monad m) => MonadSupply (SupplyT m) where
  fresh = SupplyT $ do
    i <- get
    put (i + 1)
    return i
  peek = SupplyT get

instance (MonadSupply m) => MonadSupply (StateT s m) where
  fresh = lift fresh
  peek = lift peek
