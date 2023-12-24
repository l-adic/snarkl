{-# LANGUAGE GADTs #-}

module Harness where

import Data.Field.Galois (GaloisField, Prime, PrimeField)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Typeable
import GHC.IO.Exception
import GHC.TypeLits (KnownNat)
import Snarkl.Compile (SimplParam)
import Snarkl.Errors
import Snarkl.Language.TExpr
import Snarkl.Toplevel
import System.IO (hPutStrLn, stderr)

-- Just interpret.
test_interp :: (Typeable ty, GaloisField k) => Comp ty k -> [Int] -> k
test_interp mf inputs =
  comp_interp mf (map fromIntegral inputs)

-- Just elaborate to TExp.
test_texp :: (Typeable ty) => Comp ty k -> IO ()
test_texp mf = (hPutStrLn stderr . show . extract_rat . lastSeq . comp_texp . texp_of_comp) mf
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
  let texp_pkg = texp_of_comp mf
      constrs = constrs_of_texp texp_pkg
   in hPutStrLn stderr $
        show $
          Set.size $
            cs_constraints constrs

-- Snarkl.Compile to constraints and simplify.
test_simplify :: (Typeable ty, GaloisField k) => Comp ty k -> IO ()
test_simplify mf =
  let texp_pkg = texp_of_comp mf
      constrs = constrs_of_texp texp_pkg
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
    exit <- snarkify_comp "test" simpl mf (map fromIntegral inputs)
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
