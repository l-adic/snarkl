{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}

module Snarkl.Example.Sudoku where

import Data.Field.Galois (GaloisField, Prime, PrimeField)
import Data.Fin (Fin, universe, weakenLeft, weakenRight)
import Data.Type.Nat (FromGHC, Nat3, Nat6, Nat9, SNatI)
import Data.Typeable (Proxy (Proxy), Typeable)
import Debug.Trace (trace)
import GHC.TypeLits (KnownNat)
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

type SudokuSet k = TExp ('TVec Nat9 'TField) k

validPuzzle ::
  (GaloisField k) =>
  Comp 'TBool k
validPuzzle = do
  sol <- input_arr2 @'TField 9 9
  let p = unsafe_cast sol
  set <- sudokuSet
  rowsValid <- do
    rowsValid <- traverseV (isValidRow set) p
    allV rowsValid
  colsValid <- do
    pt <- transpose p
    validCols <- traverseV (isValidRow set) pt
    allV validCols
  boxesValid <- do
    bs <- asBoxes p
    validateBoxes set bs
  return $ rowsValid && colsValid && boxesValid
  where
    sudokuSet = do
      as <- vec (Proxy @Nat9)
      _ <- forall [0 .. 8] $ \i ->
        setV (as, i) (fromField $ fromIntegral i)
      return as

inSet :: TExp ('TVec Nat9 'TField) k -> Comp ('TFun 'TField 'TBool) k
inSet ns = lambda $ \a -> do
  f <- lambda $ \acc -> lambda $ \n -> return (acc || eq n a)
  foldlV f true ns

sudokuSet :: (GaloisField k) => Comp ('TVec Nat9 'TField) k
sudokuSet = do
  as <- vec (Proxy @Nat9)
  _ <- forall [0 .. 8] $ \i ->
    setV (as, i) (fromField $ fromIntegral i)
  return as

frequency ::
  (GaloisField k) =>
  TExp 'TField k ->
  TExp ('TVec Nat9 'TField) k ->
  Comp 'TField k
frequency a as = do
  f <- lambda $ \acc ->
    lambda $ \i ->
      return $
        ifThenElse_aux (eq i a) (acc + fromField 1) acc
  foldlV f (fromField 0) as

isValidRow ::
  (GaloisField k) =>
  SudokuSet k ->
  TExp ('TVec Nat9 'TField) k ->
  Comp 'TBool k
isValidRow ss as = do
  f <- lambda $ \n -> do
    freq <- frequency n as
    return $ freq `eq` fromField 1
  appearsOnceInRow <- traverseV (apply f) ss
  allV appearsOnceInRow

asBoxes ::
  (Typeable ty) =>
  TExp ('TVec Nat9 ('TVec Nat9 ty)) k ->
  Comp ('TVec Nat3 ('TVec Nat3 ('TVec Nat3 ('TVec Nat3 ty)))) k
asBoxes as = traverseV chunkV as >>= chunkV

validateBoxes ::
  (GaloisField k) =>
  SudokuSet k ->
  TExp ('TVec Nat3 ('TVec Nat3 ('TVec Nat3 ('TVec Nat3 'TField)))) k ->
  Comp 'TBool k
validateBoxes ss as = do
  bs <- vec (Proxy @Nat9)
  _ <- forall2 (universe @Nat3, universe @Nat3) $ \i j -> do
    box <- getV2 (as, i, j)
    b <- concatV @Nat3 @Nat3 box
    validBox <- isValidRow ss b
    let idx :: Fin Nat9
        idx = 3 P.* weakenLeft (Proxy @Nat6) i P.+ weakenRight (Proxy @Nat6) j
    setV (bs, idx) validBox
  allV bs

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