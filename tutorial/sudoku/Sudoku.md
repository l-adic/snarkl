# Sudoku Verifier

We are going to walk through the classic ZK example of the [soduku](https://en.wikipedia.org/wiki/Sudoku) verifier. In this example, we write a program which can verify that a proposed solution is the valid solution to a given sudoku puzzle. A valid solution means that:

1. Certain square assignments are fixed by the puzzle at the outset. These assignments correspond to the public inputs and must appear in the proposed solution.
2. Each row and column is a permutation of the numbers `[1..9]`.
3. Each 3x3 subgrid is a permuation of the numbers `[1..9]`.

The sudoku verifier is a useful example because it demonstrates some advanced features of Snarkl such as:

1. Language constructs like function definitions (e.g. `lambda`s) and vector operations (e.g. `map`, `fold`, `traverse`)
2. As we are using vectors, all sizes are statically known and the vector types are size indexed. This requires using features of the [fin](https://hackage.haskell.org/package/fin-0.3) package to manage things like position indexing.
3. There are private inputs supplied during witness generation which are not solved for during program execution. As the verifier program only verifies, the puzzle solution must be found "out of band".

We will also show how to use this program as a standalone CLI application -- Snarkl provides a simple way to wire this up.

## Setup

The following import statements and language extensions are required:


```haskell
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -fno-warn-unused-do-bind #-}

{-# HLINT ignore "Use if" #-}

module Sudoku where

import Prelude (Eq, fromIntegral, fromInteger, show, ($), (<>), (==), otherwise)
import qualified Prelude as P

import Data.Field.Galois (GaloisField)
import Data.List (length, take)
import Data.Proxy (Proxy(..))
import Data.Fin (universe)
import Data.Type.Nat (Nat3, Nat9, SNatI, reify)
import Snarkl.Language.Prelude
import Snarkl.Language.Vector (Vector)
import qualified Snarkl.Language.Vector as Vec
import Snarkl.Language.Matrix (Matrix)
import qualified Snarkl.Language.Matrix as Matrix
```

Everything is straightforward with the exception of the `RebindableSyntax` extension and the related `HLint` pragma `ignore "Use if"`. See the [note on rebindable syntax](../README.md#rebindablesyntax-extension-etc) for an explanation.

We use the following definition for a `Board` and `SudokuSet` 

```haskell
-- The outer indices run over the columns, each element corresponds to a row.
type Board = Matrix Nat9 Nat9 'TField

type SudokuSet = Vector Nat9 'TField

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
  f <- lambda $ \acc ->
         lambda $ \b -> 
          return $ acc || (a `eq` b)
  Vec.foldl f false sudokuSet
```

Here we are intializing a `SudokuSet`, with the numbers `[1..9]`, and give a function that can check membership in this set by brute force.

For an explanation of the `GaloisField` constraint, see the [note on Galois fields](../README.md#galois-fields).

For an explanation on why `'TField` and `'TBool` (as well as other primitive types) use "tick marks", see [this note](../README.md/#types-with-tick-marks)


## Validating the Board

According to the validation criterion above, we need to check that each row and column is 
a permutation of the numbers `[1..9]`. We iterate over the elements and check:
1. Is the element in the valid range?
2. Has the element appeared previously in the list?

```haskell
-- | Make sure that this input is a permutation of the SudokuSet
isPermutation ::
  forall n k.
  SNatI n => 
  Eq k =>
  TExp SudokuSet k ->
  TExp (Vector n 'TField) k ->
  Comp 'TBool k
isPermutation ss as = do
  Vec.traverseWithIndex f as >>= Vec.all
    where
      f i ai 
        | i == 0 = isInSudokuSet ss ai
        | otherwise = do 
            isValidNumber <- isInSudokuSet ss ai
            let js = take (fromIntegral i) $ universe @n
            reify (fromIntegral $ length js) $ \(_ :: Proxy m) ->  do 
              -- 'iChecks ! j' := 'as ! i == as ! j' `
              iChecks <- Vec.vec @m
              forall js $ \j -> do
                aj <- Vec.get (as, j)
                Vec.set (iChecks, fromIntegral j) (eq ai aj)
              existsDuplicate <- Vec.any iChecks
              return $ isValidNumber && not existsDuplicate
```

## Validating the Boxes

We need some similar code in order to treat the 3x3 boxes. We can use `Vector.chunk` to break up the board into smaller boxes. `Vector.chunk` uses the size types and type inference to take care of the details:

```haskell
-- | A 'Box' is a 3x3 grid that must be completed with a permutation of [1..9]
type Box = Matrix Nat3 Nat3 'TField

-- | 'BoxGrid' is a view of the board as a 3x3 grid of 'Box'es
type BoxGrid = Matrix Nat3 Nat3 Box

mkBoxes :: TExp Board k -> Comp BoxGrid k
mkBoxes as = Vec.traverse Vec.chunk as >>= Vec.chunk
```

We can then traverse the grid of boxes using our `isPermutation` function on each box:

```haskell
-- | Vaidate a 3x3 square, i.e. the numbers in the square form a valid set.
validateBoxes ::
  forall k. 
  Eq k =>
  TExp SudokuSet k ->
  TExp BoxGrid k ->
  Comp 'TBool k
validateBoxes ss as = 
  let isValidBox box = Vec.concat box >>= isPermutation ss
  in Matrix.traverse isValidBox as >>= Matrix.all
```


## Getting Input

We want our sudoku verifier code to be reusable for any sudoku puzzle. We can do this by accepting public input for all 81 squares and allowing the value `0` to represent a blank square. Since this input is public, validation that the input belongs to the set `[0 .. 9]` is not required in Snarkl.

In order to give the solution for a concrete puzzle, we need to allow the prover to override the input variables initialized with `0` with their own private input.


```haskell
mkBoard
  :: GaloisField k =>
  Comp Board k
mkBoard = do
  inputBoard <- Matrix.inputMatrix @Nat9 @Nat9
  let f n m input = do
        if return $ input `eq` fromField 0
             then fresh_private_input $ mkVarName n m
             else return input
  Matrix.traverseWithIndex f inputBoard
  where
    mkVarName i j = "x_" <> show (i,j)
```


## Finishing Up

We now have all the pieces to write the top level validator. We pass in the indices for the blank squares that the prover will provide. We then create the board and run each of our validation criteria over that board.

```haskell
validatePuzzle ::
  (GaloisField k) =>
  Comp 'TBool k
validatePuzzle = do
  ss <- mkSudokuSet
  board <- mkBoard
  rowsValid <- Vec.traverse (isPermutation ss) board >>= Vec.all
  colsValid <- Vec.transpose board >>= Vec.traverse (isPermutation ss) >>= Vec.all
  boxesValid <- mkBoxes board >>= validateBoxes ss
  return $ rowsValid && colsValid && boxesValid
```

At this point we can either embed this computation in a larger program, or we can generate a standalone CLI to manage the r1cs and witness generation. To see how to use the CLI, [continue on with the tutorial](./Main.md)