{-# LANGUAGE RebindableSyntax #-}

module Snarkl.Example.Tree where

import Data.Field.Galois (Prime)
import Data.Typeable
import GHC.TypeLits (KnownNat)
import Snarkl.Language.Syntax
import Snarkl.Language.SyntaxMonad
import Snarkl.Language.TExpr
import Prelude hiding
  ( fromRational,
    negate,
    return,
    (&&),
    (*),
    (+),
    (-),
    (/),
    (>>),
    (>>=),
  )

type TF a = 'TFSum ('TFConst 'TUnit) ('TFProd ('TFConst a) ('TFProd 'TFId 'TFId))

type TTree a = 'TMu (TF a)

type Rat p = TExp 'TField (Prime p)

type Tree a p = TExp (TTree a) (Prime p)

leaf :: (Typeable a, KnownNat p) => Comp (TTree a) p
leaf = do
  t <- inl unit
  roll t

node :: (Typeable a, KnownNat p) => TExp a (Prime p) -> Tree a p -> Tree a p -> Comp (TTree a) p
node v t1 t2 = do
  p <- pair t1 t2
  p' <- pair v p
  t <- inr p'
  roll t

case_tree ::
  ( Typeable a,
    KnownNat p,
    Typeable a1,
    Zippable a1 p
  ) =>
  Tree a p ->
  Comp a1 p ->
  (TExp a (Prime p) -> Tree a p -> Tree a p -> Comp a1 p) ->
  Comp a1 p
case_tree t f_leaf f_node = do
  t' <- unroll t
  case_sum (\_ -> f_leaf) go t'
  where
    go p' = do
      v <- fst_pair p'
      p <- snd_pair p'
      t1 <- fst_pair p
      t2 <- snd_pair p
      f_node v t1 t2

map_tree ::
  ( Typeable a,
    Typeable a1,
    Zippable a1 p,
    Derive a1 p,
    KnownNat p
  ) =>
  (TExp a (Prime p) -> State (Env p) (TExp a1 (Prime p))) ->
  TExp (TTree a) (Prime p) ->
  Comp (TTree a1) p
map_tree f t =
  fix go t
  where
    go self t0 = do
      case_tree
        t0
        leaf
        ( \v t1 t2 -> do
            v' <- f v
            t1' <- self t1
            t2' <- self t2
            node v' t1' t2'
        )

{------------------------------------------------
 Test cases
 ------------------------------------------------}

tree1 :: (KnownNat p) => Comp (TTree 'TField) p
tree1 = do
  b <- fresh_input
  l1 <- leaf
  l2 <- leaf
  t1' <- if return b then node (toP 77) l1 l2 else leaf
  l3 <- leaf
  t2 <- node (toP 2) t1' l3
  return t2

tree_test1 :: (KnownNat p) => Comp 'TField p
tree_test1 = do
  t <- tree1
  case_tree
    t
    (return (toP 99))
    ( \_ tl _ -> do
        case_tree
          tl
          (return (toP 88))
          ( \v _ _ -> do
              return v
          )
    )
