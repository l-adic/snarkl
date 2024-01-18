{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Snarkl.Constraint.SimplMonad
  ( SEnv (..),
    unite_vars,
    bind_var,
    root_of_var,
    bind_of_var,
    assgn_of_vars,
    SolveMode (..),
  )
where

import Control.Monad.State
import Data.Field.Galois (GaloisField)
import qualified Data.Map as Map
import Snarkl.Common (Assgn (Assgn), Var)
import qualified Snarkl.Constraint.UnionFind as UF

----------------------------------------------------------------
--                  Simplifier State Monad                    --
----------------------------------------------------------------

data SolveMode = UseMagic | JustSimplify
  deriving (Show)

data SEnv a = SEnv
  { -- | Equalities among variables,
    -- together with a partial map from variables
    -- to constants (hidden inside the "Snarkl.Constraint.UnionFind"
    -- data structure).
    eqs :: UF.UnionFind Var a,
    -- | Use Magic only in 'solve_mode'.
    -- In simplify mode, only forced equalities
    -- should be propagated.
    solve_mode :: SolveMode
  }
  deriving (Show)

-- | Unify variables 'x' and 'y'.
unite_vars :: (GaloisField a) => Var -> Var -> State (SEnv a) ()
unite_vars x y =
  do modify (\senv -> senv {eqs = UF.unite (eqs senv) x y})

-- | Bind variable 'x' to 'c'.
bind_var :: (GaloisField a) => (Var, a) -> State (SEnv a) ()
bind_var (x, c) =
  do
    rx <- root_of_var x
    senv <- get
    let eqs' = (eqs senv) {UF.extras = Map.insert rx c (UF.extras $ eqs senv)}
    put $ senv {eqs = eqs'}

-- | Return 'x''s root (the representative of its equivalence class).
root_of_var :: (GaloisField a) => Var -> State (SEnv a) Var
root_of_var x =
  do
    senv <- get
    let (rx, eqs') = UF.root (eqs senv) x
    put (senv {eqs = eqs'})
    return rx

-- | Return the binding associated with variable 'x', or 'x''s root
-- if no binding exists.
bind_of_var :: (GaloisField a) => Var -> State (SEnv a) (Either Var a)
bind_of_var x =
  do
    rx <- root_of_var x
    senv <- get
    case UF.extraOf (eqs senv) rx of
      Nothing -> return $ Left rx
      Just c -> return $ Right c

-- | Construct a partial assignment from 'vars' to field elements.
assgn_of_vars :: (GaloisField a) => [Var] -> State (SEnv a) (Assgn a)
assgn_of_vars vars =
  do
    binds <- mapM bind_of_var vars
    return
      $ Assgn
      $ Map.fromList
      $ concatMap
        ( \(x, ec) -> case ec of
            Left _ -> []
            Right c -> [(x, c)]
        )
      $ zip vars binds
