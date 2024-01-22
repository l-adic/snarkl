{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module Snarkl.Language.Matrix
  ( Matrix,
    matrix,
    inputMatrix,
    get,
    set,
    map,
    fold,
    traverse,
    traverseWithIndex,
    transpose,
    all,
    any,
  )
where

import Data.Fin (Fin, universe)
import Data.Nat (Nat)
import Data.Type.Nat (SNatI, reflectToNum)
import Data.Typeable (Proxy (Proxy), Typeable)
import Snarkl.AST.TExpr (TExp (TEApp))
import Snarkl.Language.Prelude
  ( Comp,
    Ty (TBool, TFun, TUnit, TVec),
    apply,
    arr2,
    false,
    forall2,
    lambda,
    return,
    true,
    unsafe_cast,
    (&&),
    (>>=),
    (||),
  )
import qualified Snarkl.Language.Prelude as Snarkl
import Prelude hiding (all, any, concat, foldl, map, return, traverse, (&&), (*), (>>=), (||))

type Matrix n m ty = 'TVec n ('TVec m ty)

matrix ::
  forall (n :: Nat) (m :: Nat) (ty :: Ty) k.
  (SNatI n) =>
  (SNatI m) =>
  (Typeable ty) =>
  Comp (Matrix n m ty) k
matrix = do
  let n = reflectToNum (Proxy @n)
      m = reflectToNum (Proxy @m)
  a <- Snarkl.arr2 @ty n m
  return $ unsafe_cast a

inputMatrix ::
  forall (n :: Nat) (m :: Nat) (ty :: Ty) k.
  (SNatI n) =>
  (SNatI m) =>
  (Typeable ty) =>
  Comp (Matrix n m ty) k
inputMatrix = do
  let n = reflectToNum (Proxy @n)
      m = reflectToNum (Proxy @m)
  a <- Snarkl.input_arr2 @ty n m
  return $ unsafe_cast a

get ::
  (Typeable ty) =>
  (SNatI n) =>
  (SNatI m) =>
  (TExp (Matrix n m ty) k, Fin n, Fin m) ->
  Comp ty k
get (a, i, j) =
  Snarkl.get2 (unsafe_cast a, fromIntegral i, fromIntegral j)

set ::
  (Typeable ty) =>
  (SNatI n) =>
  (SNatI m) =>
  (TExp (Matrix n m ty) k, Fin n, Fin m) ->
  TExp ty k ->
  Comp 'TUnit k
set (a, i, j) e =
  Snarkl.set2 (unsafe_cast a, fromIntegral i, fromIntegral j) e

map ::
  forall a b (n :: Nat) (m :: Nat) k.
  (SNatI n) =>
  (SNatI m) =>
  (Typeable a) =>
  (Typeable b) =>
  TExp ('TFun a b) k ->
  TExp (Matrix n m a) k ->
  Comp (Matrix n m b) k
map f a = do
  b <- matrix
  _ <- forall2 (universe @n, universe @m) $ \i j -> do
    ai <- get (a, i, j)
    bi <- apply f ai
    set (b, i, j) bi
  return b

fold ::
  forall a b (n :: Nat) (m :: Nat) k.
  (SNatI n) =>
  (SNatI m) =>
  (Typeable a) =>
  (Typeable b) =>
  TExp ('TFun b ('TFun a b)) k ->
  TExp b k ->
  TExp (Matrix n m a) k ->
  Comp b k
fold f b0 as = do
  let is = [(i, j) | i <- universe @n, j <- universe @m]
  go is b0
  where
    go ns acc = case ns of
      [] -> return acc
      ((i, j) : rest) -> do
        ai <- get (as, i, j)
        let acc' = TEApp (TEApp f acc) ai
        go rest acc'

traverse ::
  forall a b (n :: Nat) (m :: Nat) k.
  (SNatI n) =>
  (SNatI m) =>
  (Typeable a) =>
  (Typeable b) =>
  (TExp a k -> Comp b k) ->
  TExp (Matrix n m a) k ->
  Comp (Matrix n m b) k
traverse f as = do
  bs <- matrix
  _ <- forall2 (universe @n, universe @m) $ \i j -> do
    ai <- get (as, i, j)
    bi <- f ai
    set (bs, i, j) bi
  return bs

traverseWithIndex ::
  forall a b (n :: Nat) (m :: Nat) k.
  (SNatI n) =>
  (SNatI m) =>
  (Typeable a) =>
  (Typeable b) =>
  (Fin n -> Fin m -> TExp a k -> Comp b k) ->
  TExp (Matrix n m a) k ->
  Comp (Matrix n m b) k
traverseWithIndex f as = do
  bs <- matrix
  _ <- forall2 (universe @n, universe @m) $ \i j -> do
    ai <- get (as, i, j)
    bi <- f i j ai
    set (bs, i, j) bi
  return bs

transpose ::
  forall (n :: Nat) (m :: Nat) k ty.
  (Typeable ty) =>
  (SNatI n) =>
  (SNatI m) =>
  TExp (Matrix n m ty) k ->
  Comp (Matrix m n ty) k
transpose asV = do
  let n = reflectToNum (Proxy @n)
      m = reflectToNum (Proxy @m)
      as = unsafe_cast asV
      is = fromIntegral <$> universe @n
      js = fromIntegral <$> universe @n
  bs <- arr2 @ty m n
  _ <- forall2 (is, js) $ \i j -> do
    ai <- Snarkl.get2 (as, i, j)
    Snarkl.set2 (bs, j, i) ai
  return $ unsafe_cast bs

all ::
  (SNatI n) =>
  (SNatI m) =>
  TExp (Matrix n m 'TBool) k ->
  Comp 'TBool k
all as = do
  f <- lambda $ \acc ->
    lambda $ \x ->
      return $ acc && x
  fold f true as

any ::
  (SNatI n) =>
  (SNatI m) =>
  TExp (Matrix n m 'TBool) k ->
  Comp 'TBool k
any as = do
  f <- lambda $ \acc ->
    lambda $ \x ->
      return $ acc || x
  fold f false as
