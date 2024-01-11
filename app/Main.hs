{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import Data.Field.Galois (PrimeField)
import qualified Data.Map as Map
import Data.Typeable (Typeable)
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Example.Sudoku
import Snarkl.Field (F_BN128)
import Snarkl.Language (Comp)
import Snarkl.Syntax
import Snarkl.Toplevel
  ( Result (..),
    SimplParam (..),
    execute,
    execute',
    mkInputsFilePath,
    mkR1CSFilePath,
    mkWitnessFilePath,
    serializeInputsAsJson,
    serializeR1CSAsJson,
    serializeWitnessAsJson,
  )
import qualified Test.Snarkl.Unit.Programs as Programs

main :: IO ()
main = do
  executeAndWriteArtifacts
    "./snarkl-output"
    "sudoku"
    Simplify
    validPuzzle
    (fromIntegral @_ @F_BN128 <$> exampleValidPuzzle)
    (fromIntegral @_ @F_BN128 <$> exampleValidPuzzleHiddenInput)

executeAndWriteArtifacts ::
  (Typeable ty) =>
  (PrimeField k) =>
  FilePath ->
  String ->
  SimplParam ->
  Comp ty k ->
  [k] ->
  Map.Map String k ->
  IO ()
executeAndWriteArtifacts fp name simpl mf inputs known = do
  let Result {result_sat = isSatisfied, result_r1cs = r1cs, result_witness = wit} = execute' simpl mf inputs known
  unless isSatisfied $ failWith $ ErrMsg "R1CS is not satisfied"
  let r1csFP = mkR1CSFilePath fp name
  LBS.writeFile r1csFP (serializeR1CSAsJson r1cs)
  putStrLn $ "Wrote R1CS to file " <> r1csFP
  let witnessFP = mkWitnessFilePath fp name
  LBS.writeFile witnessFP (serializeWitnessAsJson r1cs wit)
  putStrLn $ "Wrote witness to file " <> witnessFP
  let inputsFP = mkInputsFilePath fp name
  LBS.writeFile inputsFP (serializeInputsAsJson r1cs inputs)
  putStrLn $ "Wrote inputs to file " <> inputsFP

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

exampleValidPuzzleHiddenInput ::
  Map.Map String Int
exampleValidPuzzleHiddenInput =
  Map.fromList
    [ ("sudokuVar-0", 2),
      ("sudokuVar-1", 1),
      ("sudokuVar-2", 5),
      ("sudokuVar-3", 3),
      ("sudokuVar-4", 7)
    ]

exampleValidPuzzle :: [Int]
exampleValidPuzzle =
  (\a -> a Prelude.- 1)
    <$> [ 6,
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