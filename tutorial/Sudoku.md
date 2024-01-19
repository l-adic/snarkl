# Example: Sudoku Verifier

```haskell
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module Snarkl.Example.Sudoku where

import Data.Field.Galois (GaloisField, Prime, PrimeField)
import Data.Fin (Fin, toNat, universe, weakenLeft, weakenRight)
import Data.Type.Nat (FromGHC, Nat3, Nat6, Nat9, SNatI, reify)
import Data.Typeable (Proxy (Proxy), Typeable)
import Debug.Trace (trace)
import GHC.TypeLits (KnownNat)
import Snarkl.AST.SyntaxMonad (foldl, fresh_known_assignment, te_assert)
import Snarkl.Syntax
import Prelude hiding
  ( all,
    concat,
    foldl,
    fromRational,
    negate,
    not,
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

validPuzzle ::
  (GaloisField k) =>
  [Int] ->
  Comp 'TBool k
validPuzzle blanks = do
  input <- vec (Proxy @(FromGHC 81))
  sudokuSet <- sudokuSet
  _ <- forall (universe @(FromGHC 81)) $ \i -> do
    vi <- fresh_input
    setV (input, fromIntegral i) vi
    -- Check to see if this index corresponds to a hidden input,
    -- i.e. a square which was empty in the original puzzle formulation
    -- but was filled in in the course of solving it.
    -- If it is, give it the assignment from the puzzle solution.
    case fromIntegral i `elem` blanks of
      True -> do
        v <- fresh_known_assignment ("x" <> show i)
        vi <- fresh_var
        n <- isInSudokuSet sudokuSet v
        te_assert vi n
        setV (input, fromIntegral i) v
      False -> return unit
  p <- chunkV @Nat9 @Nat9 input
  rowsValid <- do
    rowsValid <- traverseV isValidSet p
    allV rowsValid
  colsValid <- do
    pt <- transpose p
    validCols <- traverseV isValidSet pt
    allV validCols
  boxesValid <- do
    bs <- asBoxes p
    validateBoxes bs
  return $ rowsValid && colsValid && boxesValid
  where
    sudokuSet :: (GaloisField k) => Comp SudokuSet k
    sudokuSet = do
      ss <- vec (Proxy @Nat9)
      forall (universe @Nat9) $ \i ->
        setV (ss, i) (fromField $ 1 P.+ fromIntegral i)
      return ss

type SudokuSet = 'TVec Nat9 'TField

isValidSet ::
  TExp ('TVec Nat9 'TField) k ->
  Comp 'TBool k
isValidSet as = do
  bs <- vec (Proxy @Nat9)
  _ <- setV (bs, 0) true
  _ <- forall (drop 1 $ universe @Nat9) $ \i -> do
    a <- getV (as, i)
    let js = take (fromIntegral i) $ universe @Nat9
    reify (fromIntegral $ length js) $ \pj -> do
      iChecks <- vec pj
      _ <- forall js $ \j -> do
        b <- getV (as, j)
        setV (iChecks, fromIntegral j) (not $ eq a b)
      bi <- allV iChecks
      setV (bs, i) bi
  allV bs

isInSudokuSet ::
  TExp SudokuSet k ->
  TExp 'TField k ->
  Comp 'TBool k
isInSudokuSet sudokuSet a = do
  bs <- vec (Proxy @Nat9)
  _ <- forall (universe @Nat9) $ \i -> do
    si <- getV (sudokuSet, i)
    setV (bs, i) (eq si a)
  anyV bs

asBoxes ::
  (Typeable ty) =>
  TExp ('TVec Nat9 ('TVec Nat9 ty)) k ->
  Comp ('TVec Nat3 ('TVec Nat3 ('TVec Nat3 ('TVec Nat3 ty)))) k
asBoxes as = traverseV chunkV as >>= chunkV

validateBoxes ::
  TExp ('TVec Nat3 ('TVec Nat3 ('TVec Nat3 ('TVec Nat3 'TField)))) k ->
  Comp 'TBool k
validateBoxes as = do
  bs <- vec (Proxy @Nat9)
  _ <- forall2 (universe @Nat3, universe @Nat3) $ \i j -> do
    box <- getV2 (as, i, j)
    b <- concatV @Nat3 @Nat3 box
    validBox <- isValidSet b
    let idx :: Fin Nat9
        idx = 3 P.* weakenLeft (Proxy @Nat6) i P.+ weakenRight (Proxy @Nat6) j
    setV (bs, idx) validBox
  allV bs
```
