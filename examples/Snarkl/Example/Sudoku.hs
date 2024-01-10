{-# LANGUAGE RebindableSyntax #-}

module Snarkl.Example.Sudoku where

import Data.Field.Galois (GaloisField, Prime, PrimeField)
import Data.Typeable
import GHC.TypeLits (KnownNat)
import Snarkl.Compile
import Snarkl.Example.List
import Snarkl.Language.SyntaxMonad (arrLen, foldl)
import Snarkl.Syntax
import Prelude hiding
  ( all,
    concat,
    foldl,
    fromRational,
    negate,
    return,
    traverse,
    (&&),
    (*),
    (+),
    (-),
    (/),
    (>>),
    (>>=),
    (||),
  )
import qualified Prelude as P

inSet :: (PrimeField k) => TExp ('TArr 'TField) k -> Comp ('TFun 'TField 'TBool) k
inSet ns = lambda $ \a -> do
  f <- lambda $ \acc -> lambda $ \n -> return (acc || eq n a)
  foldl f true ns

sudokuSet :: (GaloisField k) => Comp ('TArr 'TField) k
sudokuSet = do
  as <- arr 9
  _ <- forall [0 .. 8] $ \i ->
    set (as, i) (fromField $ fromIntegral i)
  return as

frequency ::
  (PrimeField k) =>
  TExp 'TField k ->
  TExp ('TArr 'TField) k ->
  Comp 'TField k
frequency a as = do
  f <- lambda $ \acc ->
    lambda $ \i ->
      return $
        ifThenElse_aux (eq i a) (acc + fromField 1) acc
  foldl f (fromField 0) as

isValidRow ::
  (PrimeField k) =>
  TExp ('TArr 'TField) k ->
  Comp 'TBool k
isValidRow as = do
  f <- lambda $ \n -> do
    freq <- frequency n as
    return $ freq `eq` fromField 1
  ss <- sudokuSet
  appearsOnceInRow <- traverse (apply f) ss
  all appearsOnceInRow

transpose ::
  (PrimeField k) =>
  (Typeable ty) =>
  TExp ('TArr ('TArr ty)) k ->
  Comp ('TArr ('TArr ty)) k
transpose as = do
  _len <- arrLen as
  let len = coerceToInt _len
  a0 <- get (as, 0)
  _len' <- arrLen a0
  let len' = coerceToInt _len
  bs <- arr2 len' len
  _ <- forall2 ([0 .. dec len], [0 .. dec len']) $ \i j -> do
    ai <- get2 (as, i, j)
    set2 (bs, j, i) ai
  return bs

concat :: (PrimeField k, Typeable ty) => TExp ('TArr ('TArr ty)) k -> Comp ('TArr ty) k
concat as = do
  _len <- arrLen as
  let outerLen = coerceToInt _len
  a0 <- get (as, 0)
  _len' <- arrLen a0
  let innerLen = coerceToInt _len'
  -- outer len is really 3, inner as well
  bs <- arr (outerLen P.* innerLen)
  _ <- forall2 ([0 .. dec outerLen], [0 .. dec innerLen]) $ \i j -> do
    ai <- get2 (as, i, j)
    -- idx ranges in {i * 3 + j | i <- [0..2], j <- [0..2]}
    -- == {0, 1, 2, 3, 4, 5, 6, 7, 8}
    let idx = (outerLen P.* i) P.+ j
    set (bs, idx) ai
  return bs

asBoxes ::
  (PrimeField k) =>
  (Typeable ty) =>
  TExp ('TArr ('TArr ty)) k ->
  Comp ('TArr ('TArr ('TArr ('TArr ty)))) k
asBoxes as = traverse (chunk 3) as >>= chunk 3

validBoxes ::
  (PrimeField k) =>
  TExp ('TArr ('TArr ('TArr ('TArr 'TField)))) k ->
  Comp 'TBool k
validBoxes as = do
  bs <- arr 9
  _ <- forall2 ([0 .. 2], [0 .. 2]) $ \i j -> do
    box <- get2 (as, i, j)
    b <- concat box
    validBox <- isValidRow b
    set (bs, 3 P.* i P.+ j) validBox
  all bs

validPuzzle ::
  (PrimeField k) =>
  Comp 'TBool k
validPuzzle = do
  sol <- arr 81
  _ <- forall [0 .. 80] $ \i -> do
    xi <- fresh_input
    set (sol, i) xi
  p <- chunk 9 sol
  rowsValid <- traverse isValidRow p >>= all
  colsValid <- transpose p >>= traverse isValidRow >>= all
  boxesValid <- asBoxes p >>= validBoxes
  return $ rowsValid && colsValid && boxesValid

{-

[2, 1, 5, 3, 7, 6, 9, 8, 4,
 3, 6, 4, 9, 8, 1, 2, 5, 7,
 7, 8, 9, 2, 4, 5, 1, 6, 3,
 4, 5, 3, 1, 2, 9, 6, 7, 8,
 6, 2, 7, 5, 3, 8, 4, 1, 9,
 1, 9, 8, 7, 6, 4, 5, 3, 2,
 5, 7, 2, 4, 1, 3, 8, 9, 6,
 8, 3, 1, 6, 9, 2, 7, 4, 5,
 9, 4, 6, 8, 5, 7, 3, 2, 1]

-}

exampleValidPuzzle :: [Int]
exampleValidPuzzle =
  (\a -> a P.- 1)
    <$> [ 2,
          1,
          5,
          3,
          7,
          6,
          9,
          8,
          4,
          3,
          6,
          4,
          9,
          8,
          1,
          2,
          5,
          7,
          7,
          8,
          9,
          2,
          4,
          5,
          1,
          6,
          3,
          4,
          5,
          3,
          1,
          2,
          9,
          6,
          7,
          8,
          6,
          2,
          7,
          5,
          3,
          8,
          4,
          1,
          9,
          1,
          9,
          8,
          7,
          6,
          4,
          5,
          3,
          2,
          5,
          7,
          2,
          4,
          1,
          3,
          8,
          9,
          6,
          8,
          3,
          1,
          6,
          9,
          2,
          7,
          4,
          5,
          9,
          4,
          6,
          8,
          5,
          7,
          3,
          2,
          1
        ]

--------------------------------------------------------------------------------

traverse ::
  (Typeable ty1) =>
  (Typeable ty2) =>
  (PrimeField k) =>
  (TExp ty1 k -> Comp ty2 k) ->
  TExp ('TArr ty1) k ->
  Comp ('TArr ty2) k
traverse f a = do
  lenK <- arrLen a
  let len = coerceToInt lenK
  b <- arr len
  _ <-
    forall
      [0 .. dec len]
      ( \i -> do
          ai <- get (a, i)
          bi <- f ai
          set (b, i) bi
      )
  return b

chunk ::
  (Typeable ty) =>
  (PrimeField k) =>
  Int ->
  TExp ('TArr ty) k ->
  Comp ('TArr ('TArr ty)) k
chunk n as = do
  _len <- arrLen as
  let len = coerceToInt _len
      indices = [0 .. dec len]
      chunks_ = chunks n indices
      nChunks = P.length chunks_
      chunkSize = len `div` n
  bs <- arr2 nChunks chunkSize
  _ <- forall2 ([0 .. dec nChunks], [0 .. dec chunkSize]) $ \i j -> do
    let idx = nChunks P.* i P.+ j
    ai <- get (as, idx)
    set2 (bs, i, j) ai
  return bs

chunks :: Int -> [a] -> [[a]]
chunks n as =
  let (xs, ys) = splitAt n as
   in case ys of
        [] -> [xs]
        _ -> xs : chunks n ys

all ::
  (PrimeField k) =>
  TExp ('TArr 'TBool) k ->
  Comp 'TBool k
all as = do
  f <- lambda $ \acc ->
    lambda $ \x ->
      return $ acc && x
  foldl f true as
