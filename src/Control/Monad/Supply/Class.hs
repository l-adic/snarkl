module Control.Monad.Supply.Class where

import Control.Monad.State (get, put)
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
