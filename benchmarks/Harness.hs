{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Harness where

import Control.Monad ((>>))
import qualified Data.ByteString.Lazy as LBS
import Data.Field.Galois (GaloisField, Prime, PrimeField)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Typeable
import GHC.IO.Exception
import GHC.TypeLits (KnownNat)
import Snarkl.Compile (SimplParam)
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Language.TExpr
  ( TExp (..),
    lastSeq,
  )
import Snarkl.Toplevel
  ( Comp,
    ConstraintSystem (..),
    Result (..),
    TExp (..),
    TExpPkg (..),
    comp_interp,
    compileCompToR1CS,
    compileCompToTexp,
    compileTexpToConstraints,
    do_simplify,
    execute,
    lastSeq,
    serializeR1CSAsJson,
    serializeWitnessAsJson,
    wit_of_r1cs,
  )
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Test.ArkworksBridge (CMD (CreateProof, CreateTrustedSetup, RunR1CS), runCMD)

-- Just interpret.
test_interp :: (Typeable ty, GaloisField k) => Comp ty k -> [Int] -> k
test_interp mf inputs =
  comp_interp mf (map fromIntegral inputs)

-- Just elaborate to TExp.
test_texp :: (Typeable ty) => Comp ty k -> IO ()
test_texp mf = (hPutStrLn stderr . show . extract_rat . lastSeq . comp_texp . compileCompToTexp) mf
  where
    extract_rat :: TExp ty k -> Int
    extract_rat te =
      case te of
        TEVar _ -> 0
        TEVal _ -> 1
        TEUnop _ _ -> 2
        TEBinop _ _ _ -> 3
        TEIf _ _ _ -> 4
        TEAssert _ _ -> 5
        TESeq _ _ -> 6
        TEBot -> 7

-- Just compile to constraints (no simplification yet).
test_constraints :: (Typeable ty, GaloisField k) => Comp ty k -> IO ()
test_constraints mf =
  let texp_pkg = compileCompToTexp mf
      constrs = compileTexpToConstraints texp_pkg
   in hPutStrLn stderr $
        show $
          Set.size $
            cs_constraints constrs

-- Snarkl.Compile to constraints and simplify.
test_simplify :: (Typeable ty, GaloisField k) => Comp ty k -> IO ()
test_simplify mf =
  let texp_pkg = compileCompToTexp mf
      constrs = compileTexpToConstraints texp_pkg
      (_, constrs') = do_simplify False Map.empty constrs
   in hPutStrLn stderr $
        show $
          Set.size $
            cs_constraints constrs'

-- Generate (simplified) R1CS, but don't run it yet.  (No witness is
-- generated.) Also, serialize the r1cs to stderr.
test_r1csgen :: (Typeable ty, PrimeField k) => SimplParam -> Comp ty k -> IO ()
test_r1csgen simpl mf =
  do
    r1csgen_comp "test" simpl mf

-- Same as 'test_r1cs', but also generates and serializes
-- a satisfying assignment, as well as serializing the given inputs.
test_witgen :: (Integral a, Typeable ty, PrimeField k) => SimplParam -> Comp ty k -> [a] -> IO ()
test_witgen simpl mf inputs =
  do
    witgen_comp "test" simpl mf (map fromIntegral inputs)

test_keygen :: (Typeable ty, PrimeField k) => SimplParam -> Comp ty k -> [Int] -> IO ()
test_keygen simpl mf inputs =
  do
    exit <- keygen_comp "test" simpl mf (map fromIntegral inputs)
    case exit of
      ExitSuccess -> Prelude.return ()
      ExitFailure err -> failWith $ ErrMsg $ "test_full failed with " ++ show err

test_proofgen :: (Typeable ty, PrimeField k) => SimplParam -> Comp ty k -> [Int] -> IO ()
test_proofgen simpl mf inputs =
  do
    exit <- proofgen_comp "test" simpl mf (map fromIntegral inputs)
    case exit of
      ExitSuccess -> Prelude.return ()
      ExitFailure err -> failWith $ ErrMsg $ "test_full failed with " ++ show err

-- Run libsnark on the resulting files.
test_crypto :: (Typeable ty, PrimeField k) => SimplParam -> Comp ty k -> [Int] -> IO ()
test_crypto simpl mf inputs =
  do
    exit <- runCMD $ RunR1CS "./scripts" "test" simpl mf (map fromIntegral inputs)
    case exit of
      ExitSuccess -> Prelude.return ()
      ExitFailure err -> failWith $ ErrMsg $ "test_full failed with " ++ show err

-- This function "executes" the comp two ways, once by interpreting
-- the resulting TExp and second by interpreting the resulting circuit
-- on the inputs provided. Both results are checked to match 'res'.
-- The function outputs a 'Result' that details number of variables and
-- constraints in the resulting constraint system.
test_numconstrs :: (Typeable ty, PrimeField k) => SimplParam -> Comp ty k -> [Int] -> k -> IO ()
test_numconstrs simpl mf inputs res =
  benchmark_comp (simpl, mf, map fromIntegral inputs, res)

-- | 'execute' computation, reporting error if result doesn't match
-- the return value provided by the caller. Also, serializes the
-- resulting 'R1CS'.
benchmark_comp :: (Typeable ty, PrimeField k) => (SimplParam, Comp ty k, [k], k) -> IO ()
benchmark_comp (simpl, prog, inputs, res) =
  let print_ln = print_ln_to_file stdout
      print_ln_to_file h s = (Control.Monad.>>) (hPutStrLn h s) (hFlush h)
   in case execute simpl prog inputs of
        r@(Result True _ _ res' _ _) ->
          if res == res'
            then do
              print_ln $ show r
            else
              print_ln $
                show $
                  "error: results don't match: "
                    ++ "expected "
                    ++ show res
                    ++ " but got "
                    ++ show res'
        Result False _ _ _ _ _ ->
          print_ln "error: witness failed to satisfy constraints"

keygen_comp :: (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> [k] -> IO ExitCode
keygen_comp filePrefix simpl c _ = runCMD $ CreateTrustedSetup "scripts" filePrefix simpl c

proofgen_comp :: (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> [k] -> IO ExitCode
proofgen_comp filePrefix simpl c inputs = runCMD $ CreateProof "scripts" filePrefix simpl c inputs

r1csgen_comp :: (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> IO ()
r1csgen_comp filePrefix simpl c =
  do
    let r1cs = compileCompToR1CS simpl c
        r1cs_file = "scripts/" ++ filePrefix ++ "-r1cs.jsonl"
    LBS.writeFile r1cs_file (serializeR1CSAsJson r1cs)

witgen_comp :: (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> [k] -> IO ()
witgen_comp filePrefix simpl c inputs = do
  let r1cs = compileCompToR1CS simpl c
      wit = wit_of_r1cs inputs r1cs
      r1cs_file = "scripts/" ++ filePrefix ++ "-r1cs.jsonl"
      wits_file = "scripts/" ++ filePrefix ++ "-witness.jsonl"
  LBS.writeFile r1cs_file (serializeR1CSAsJson r1cs)
  LBS.writeFile wits_file (serializeWitnessAsJson r1cs wit)
