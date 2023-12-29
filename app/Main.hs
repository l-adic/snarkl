module Main where

import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import Data.Field.Galois (PrimeField)
import Data.Typeable (Typeable)
import Snarkl.Compile (SimplParam (NoSimplify))
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Field
import Snarkl.Toplevel
import qualified Test.Snarkl.Unit.Programs as Programs

main :: IO ()
main = do
  executeAndWriteArtifacts "./output" "prog2" NoSimplify (Programs.prog2 10) [1 :: F_BN128]

executeAndWriteArtifacts :: (Typeable ty, PrimeField k) => FilePath -> String -> SimplParam -> Comp ty k -> [k] -> IO ()
executeAndWriteArtifacts fp name simpl mf inputs = do
  let Result {result_sat = isSatisfied, result_r1cs = r1cs, result_witness = wit} = execute simpl mf inputs
  unless isSatisfied $ failWith $ ErrMsg "R1CS is not satisfied"
  let r1csFP = fp <> "/" <> name <> "-r1cs.jsonl"
  LBS.writeFile r1csFP (serializeR1CSAsJson r1cs)
  putStrLn $ "Wrote R1CS to file " <> r1csFP
  let witnessFP = fp <> "/" <> name <> "-witness.jsonl"
  LBS.writeFile witnessFP (serializeWitnessAsJson r1cs wit)
  putStrLn $ "Wrote witness to file " <> witnessFP
  let inputsFP = fp <> "/" <> name <> "-inputs.jsonl"
  LBS.writeFile inputsFP (serializeInputsAsJson r1cs inputs)
  putStrLn $ "Wrote inputs to file " <> inputsFP