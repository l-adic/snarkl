# Example: Sudoku Verifier

We are going to walk through the classic ZK example of the [soduku](https://en.wikipedia.org/wiki/Sudoku) verifier. In this example, we write a program which can verify that a proposed solution is the valid solution to a given sudoku puzzle. A valid solution means that:

1. Certain square assignments are fixed by the puzzle at the outset. These assignments correspond to the public inputs and must appear in the proposed solution.
2. Each row and column is a permutation of the numbers `[1..9]`.
3. Each 3x3 subgrid is a permuation of the numbers `[1..9]`.

The sudoku verifier is a useful example because it demonstrates some advanced features of Snarkl such as:
1. Language constructs like function definitions (e.g. `lambda`s) and vector operations (e.g. `map`, `fold`, `traverse`)
2. As we are using vectors, all sizes are statically known and the vector types are size indexed. This requires using features of the [fin](https://hackage.haskell.org/package/fin-0.3) package to manage things like position indexing.
3. There are private inputs supplied during witness generation which are not solved for during program execution. As the verifier program only verifies, the puzzle solution must be found "out of band" and supplied as private input.

This example is written as a literate haskell file (i.e. this is code). You can execute the main function via `cabal run exe:sudoku` and you will be presented with a CLI tool to manage the r1cs and witness generation.

## Setup

The following import statements and language extensions are required:


```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -fno-warn-unused-do-bind #-}

{-# HLINT ignore "Use if" #-}

module Snarkl.Example.Sudoku where

import Data.Field.Galois (GaloisField)
import Data.Fin (Fin, universe, weakenLeft, weakenRight)
import Data.Type.Nat (FromGHC, Nat3, Nat6, Nat9, reify)
import Data.Typeable (Proxy (Proxy), Typeable)
import Snarkl.Language.Prelude
import Snarkl.Language.Vector as Vec
import Prelude hiding
  ( all,
    concat,
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
```

Everything is straightforward with the exception of the `RebindableSyntax` extension and the related `HLint` pragma `ignore "Use if"`. See the [note on rebindable syntax]() for an explanation.

We use the following definition for a `Board` and `SudokuSet` 

```haskell
-- The outer indices run over the columns, each element corresponds to a row.
type Board = 'TVec Nat9 ('TVec Nat9 'TField) 

type SudokuSet = 'TVec Nat9 'TField

mkSudokuSet :: (GaloisField k) => Comp SudokuSet k
mkSudokuSet = do
  ss <- Vec.vec (Proxy @Nat9)
  forall (universe @Nat9) $ \i ->
    Vec.set (ss, i) (fromField $ 1 P.+ fromIntegral i)
  return ss
```

Here we are intializing a `SudokuSet`, with the numbers `[1..9]`. For an explanation of the `GaloisField` constraint, see the [note on fields]().

## Validating the Board

According to the validation criterion above, we need to check that each row and column is 
a permutation of the numbers `[1..9]`. It is enough to iterate over the elements and check:
1. Is the element in the valid range?
2. Has the element appeared previously in the list.

As for point `(1)`, we will perform this validation at the time the input is presented, so we can focus on `(2)`:


```haskell
-- | Make sure that no number is repeated in the list. Earlier
-- checks that the numbers are in the valid range imply the set
-- is valid
isPermutation
  Eq k =>
  TExp ('TVec Nat9 'TField) k ->
  Comp 'TBool k
isPermutation as = do
  -- The check that 'as ! 0' has no duplicates in previous
  -- elements is vaccuous.
  forall (drop 1 $ universe @Nat9) $ \i -> do
    a <- Vec.get (as, i)
    let js = take (fromIntegral i) (universe @Nat9)
    forall js $ \j -> do
      b <- Vec.get (as, j)
      assert (not $ eq a b)
```




```haskell
validPuzzle ::
  (GaloisField k) =>
  [Int] ->
  Comp 'TBool k
validPuzzle blanks = do
  input <- Vec.vec (Proxy @(FromGHC 81))
  sudokuSet <- mkSudokuSet
  forall (universe @(FromGHC 81)) $ \i -> do
    vi <- fresh_public_input
    Vec.set (input, fromIntegral i) vi
    -- Check to see if this index corresponds to a private input.
    -- If it is, give it the assignment from the puzzle's solution 
    -- which is provided during witness generation.
    case fromIntegral i `elem` blanks of
      True -> do
        privInput <- fresh_private_input ("x" <> show i)
        isValidInput <- isInSudokuSet sudokuSet privInput
        fresh_var >>= \v -> do
          te_assert v isValidInput
          assert_true v
        Vec.set (input, fromIntegral i) privInput
      False -> return unit
  p <- Vec.chunk @Nat9 @Nat9 input
  rowsValid <- do
    rowsValid <- Vec.traverse isValidSet p
    Vec.all rowsValid
  colsValid <- do
    pt <- Vec.transpose p
    validCols <- Vec.traverse isValidSet pt
    Vec.all validCols
  boxesValid <- do
    bs <- asBoxes p
    validateBoxes bs
  return $ rowsValid && colsValid && boxesValid
  where


-- | Make sure that no number is repeated in the set. Earlier
-- checks that the numbers are in the valid range imply the set
-- is valid
isValidList ::
  Eq k =>
  TExp ('TVec Nat9 'TField) k ->
  Comp 'TBool k
isValidList as = do
  bs <- Vec.vec (Proxy @Nat9)
  forall (drop 1 $ universe @Nat9) $ \i -> do
    a <- Vec.get (as, i)
    let js = take (fromIntegral i) $ universe @Nat9
    reify (fromIntegral $ length js) $ \pj -> do
      iChecks <- Vec.vec pj
      forall js $ \j -> do
        b <- Vec.get (as, j)
        Vec.set (iChecks, fromIntegral j) (not $ eq a b)
      bi <- Vec.all iChecks
      Vec.set (bs, i) bi
  Vec.all bs

-- | Check that the number belongs to the valid range of numbers,
-- e.g. {1 .. 9}
isInSudokuSet ::
  TExp SudokuSet k ->
  TExp 'TField k ->
  Comp 'TBool k
isInSudokuSet sudokuSet a = do
  bs <- Vec.vec (Proxy @Nat9)
  forall (universe @Nat9) $ \i -> do
    si <- Vec.get (sudokuSet, i)
    Vec.set (bs, i) (eq si a)
  Vec.any bs

-- | Treat the 9x9 sudoku board as a 3x3 grid where every element
-- of the grid is a 3x3 square.
asBoxes ::
  (Typeable ty) =>
  TExp ('TVec Nat9 ('TVec Nat9 ty)) k ->
  Comp ('TVec Nat3 ('TVec Nat3 ('TVec Nat3 ('TVec Nat3 ty)))) k
asBoxes as = Vec.traverse Vec.chunk as >>= Vec.chunk

-- | Vaidate a 3x3 square, i.e. the numbers in the square form a valid set.
validateBoxes ::
  Eq k =>
  TExp ('TVec Nat3 ('TVec Nat3 ('TVec Nat3 ('TVec Nat3 'TField)))) k ->
  Comp 'TBool k
validateBoxes as = do
  bs <- Vec.vec (Proxy @Nat9)
  forall2 (universe @Nat3, universe @Nat3) $ \i j -> do
    box <- Vec.get2 (as, i, j)
    b <- Vec.concat @Nat3 @Nat3 box
    validBox <- isValidSet b
    let idx :: Fin Nat9
        idx = 3 P.* weakenLeft (Proxy @Nat6) i P.+ weakenRight (Proxy @Nat6) j
    Vec.set (bs, idx) validBox
  Vec.all bs
```
