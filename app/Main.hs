{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import Data.Field.Galois (PrimeField)
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
    "prog2"
    NoSimplify
    validPuzzle
    (map (fromIntegral @_ @F_BN128) exampleValidPuzzle)

executeAndWriteArtifacts :: (Typeable ty, PrimeField k) => FilePath -> String -> SimplParam -> Comp ty k -> [k] -> IO ()
executeAndWriteArtifacts fp name simpl mf inputs = do
  let Result {result_sat = isSatisfied, result_r1cs = r1cs, result_witness = wit} = execute simpl mf inputs
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
