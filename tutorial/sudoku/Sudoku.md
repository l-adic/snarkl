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
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -fno-warn-unused-do-bind #-}

{-# HLINT ignore "Use if" #-}

module Sudoku where


import Prelude (Bool(..), Eq, Int, fromIntegral, fromInteger, show, (.), ($), (<>))
import qualified Prelude as P

import Data.Field.Galois (GaloisField)
import Data.List (drop, elem, length, take)
import Data.Proxy (Proxy(..))
import Data.Fin (Fin, universe, weakenLeft)
import Data.Type.Nat (Nat3, Nat6, Nat9, reify)
import Snarkl.Language.Prelude
import Snarkl.Language.Vector as Vec
```

Everything is straightforward with the exception of the `RebindableSyntax` extension and the related `HLint` pragma `ignore "Use if"`. See the [note on rebindable syntax]() for an explanation.

We use the following definition for a `Board` and `SudokuSet` 

```haskell
-- The outer indices run over the columns, each element corresponds to a row.
type Board = 'TVec Nat9 ('TVec Nat9 'TField) 

type SudokuSet = 'TVec Nat9 'TField

-- | Smart constructor to build the set [1..9]
mkSudokuSet :: (GaloisField k) => Comp SudokuSet k
mkSudokuSet = do
  ss <- Vec.vec
  forall (universe @Nat9) $ \i ->
    Vec.set (ss, i) (fromField $ 1 P.+ fromIntegral i)
  return ss

-- | Check that a number belongs to the valid range of numbers,
-- e.g. [1 .. 9]
isInSudokuSet ::
  TExp SudokuSet k ->
  TExp 'TField k ->
  Comp 'TBool k
isInSudokuSet sudokuSet a = do
  bs <- Vec.vec
  forall (universe @Nat9) $ \i -> do
    si <- Vec.get (sudokuSet, i)
    Vec.set (bs, i) (eq si a)
  Vec.any bs

```

Here we are intializing a `SudokuSet`, with the numbers `[1..9]`. For an explanation of the `GaloisField` constraint, see the [note on fields]().
We also give a function that can check membership in this set by brute force.

## Validating the Board

According to the validation criterion above, we need to check that each row and column is 
a permutation of the numbers `[1..9]`. It is enough to iterate over the elements and check:
1. Is the element in the valid range?
2. Has the element appeared previously in the list.

As for point `(1)`, we will perform this validation at the time the input is presented, so we can focus on `(2)`:


```haskell
-- | Make sure that no number is repeated in the set. Earlier
-- checks that the numbers are in the valid range imply the set
-- is valid
isPermutation ::
  Eq k =>
  TExp ('TVec Nat9 'TField) k ->
  Comp 'TBool k
isPermutation as = do
  -- 'bs ! i == true' iff 'bs ! i' has appeared previously
  bs <- Vec.vec
  -- the check that the first element has no prior repeats
  -- is vaccuously true.
  forall (drop 1 $ universe @Nat9) $ \i -> do
    a <- Vec.get (as, i)
    let js = take (fromIntegral i) $ universe @Nat9
    reify (fromIntegral $ length js) $ \(_ :: Proxy m) -> do 
      -- 'iChecks ! j' := 'as ! i == as ! j' `
      iChecks <- Vec.vec @m
      forall js $ \j -> do
        b <- Vec.get (as, j)
        Vec.set (iChecks, fromIntegral j) (eq a b)
      bi <- Vec.any iChecks
      Vec.set (bs, i) bi
  -- assert that we didn't get any matches
  Vec.any bs >>= (return . not)
```

## Validating the Boxes

We now need some similar code in order to treat the 3x3 boxes. We can use `Vector.chunk` to break up the 2d array into
smaller boxes. `Vector.chunk` uses the size types and type inference to take care of the details. 

```haskell
-- | A 'Box' is a 3x3 grid that must be completed with a permutation of [1..9]
type Box = 'TVec Nat3 ('TVec Nat3 'TField)

-- | 'BoxGrid' is a view of the board as a 3x3 grid of 'Box'es
type BoxGrid = 'TVec Nat3 ('TVec Nat3 Box)

mkBoxes :: TExp Board k -> Comp BoxGrid k
mkBoxes as = Vec.traverse Vec.chunk as >>= Vec.chunk
```

We can then traverse the grid of boxes, using our `isPermutation` function on each box:

```haskell
-- | Vaidate a 3x3 square, i.e. the numbers in the square form a valid set.
validBoxes ::
  Eq k =>
  TExp BoxGrid k ->
  Comp 'TBool k
validBoxes as = do
  bs <- Vec.vec
  forall2 (universe @Nat3, universe @Nat3) $ \i j -> do
    box <- Vec.get2 (as, i, j)
    b <- Vec.concat @Nat3 @Nat3 box
    validBox <- isPermutation b
    let idx :: Fin Nat9
        -- we have to (safely) coerce the arithmetic to take place in for numbers less than 9
        idx = 3 P.* weakenLeft (Proxy @Nat6) i P.+ weakenLeft (Proxy @Nat6) j
    Vec.set (bs, idx) validBox
  Vec.all bs
```


## Getting Input

We want our sudoku verifier code to be reusable for any sudoku puzzle. We can do this by accepting public input
for all 81 squares and allowing the value `0` to represent a blank square. Since this input is public, validation
that the input belongs to the set `[0 .. 9]` is not required in Snarkl.

In order to give the solution for a concrete puzzle, we need to allow the prover to override the input variables
initialized with `0` with their own private input. Since this input is private, we want to validate that it
belongs to the specified `SudokuSet` in Snarkl.


```haskell
mkBoard
  :: GaloisField k =>
  TExp SudokuSet k ->
  [(Int, Int)] ->
  -- ^ The list of blank squares in (row,col) format
  Comp Board k
mkBoard ss blanks = do
  board <- Vec.inputVec2
  forall (universe @Nat9) $ \i -> 
    forall (universe @Nat9) $ \j -> 
      case elem (fromIntegral i, fromIntegral j) blanks of 
        False -> return unit
        True -> do 
          knownValue <- fresh_private_input ("x_" <> show (i,j))
          isInSudokuSet ss knownValue >>= assert
          Vec.set2 (board, i, j) knownValue
  return board
```


## Finishing Up

We now have all the pieces to write the top level validator. To create the verifier, we pass in the indices
for the blank squares that the prover will provide. We then simply create the board and run each of our
validation criteria over that board.

```haskell
validatePuzzle ::
  (GaloisField k) =>
  [(Int,Int)] -> 
  -- ^ The list of blank squares in (row,col) format
  Comp 'TBool k
validatePuzzle blanks = do
  ss <- mkSudokuSet
  board <- mkBoard ss blanks
  rowsValid <- do
    rowsValid <- Vec.traverse isPermutation board
    Vec.all rowsValid
  colsValid <- do
    validCols <- Vec.transpose board >>= Vec.traverse isPermutation
    Vec.all validCols
  boxesValid <- mkBoxes board >>= validBoxes
  return $ rowsValid && colsValid && boxesValid

```