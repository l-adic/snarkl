{-# LANGUAGE RebindableSyntax #-}

module Snarkl.Example.Tree where

import Data.Field.Galois (GaloisField, Prime)
import Data.Typeable
import GHC.TypeLits (KnownNat)
import Snarkl.Language.Prelude
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

type Rat k = TExp 'TField k

type Tree a k = TExp (TTree a) k

leaf :: (Typeable a, GaloisField k) => Comp (TTree a) k
leaf = do
  t <- inl unit
  roll t

node :: (Typeable a, GaloisField k) => TExp a k -> Tree a k -> Tree a k -> Comp (TTree a) k
node v t1 t2 = do
  p <- pair t1 t2
  p' <- pair v p
  t <- inr p'
  roll t

case_tree ::
  ( Typeable a,
    GaloisField k,
    Typeable a1,
    Zippable a1 k
  ) =>
  Tree a k ->
  Comp a1 k ->
  (TExp a k -> Tree a k -> Tree a k -> Comp a1 k) ->
  Comp a1 k
case_tree t f_leaf f_node = do
  t' <- unroll t
  case_sum (const f_leaf) go t'
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
    Zippable a1 k,
    Derive a1 k,
    GaloisField k
  ) =>
  (TExp a k -> Comp a1 k) ->
  TExp (TTree a) k ->
  Comp (TTree a1) k
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

tree1 :: (GaloisField k) => Comp (TTree 'TField) k
tree1 = do
  b <- fresh_public_input
  l1 <- leaf
  l2 <- leaf
  t1' <- if return b then node (fromField 77) l1 l2 else leaf
  l3 <- leaf
  node (fromField 2) t1' l3

tree_test1 :: (GaloisField k) => Comp 'TField k
tree_test1 = do
  t <- tree1
  case_tree
    t
    (return (fromField 99))
    ( \_ tl _ -> do
        case_tree
          tl
          (return (fromField 88))
          ( \v _ _ -> do
              return v
          )
    )
