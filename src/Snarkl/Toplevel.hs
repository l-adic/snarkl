{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Snarkl.Toplevel
  ( -- * Interpret Snarkl Computations
    comp_interp,

    -- * Desugar
    TExpPkg (..),
    texp_of_comp,

    -- * Generate Constraints
    constrs_of_texp,
    constrs_of_comp,

    -- * Generate R1CS
    r1cs_of_constrs,
    r1cs_of_texp,
    r1cs_of_comp,

    -- * Given arguments, construct a witness
    wit_of_r1cs,

    -- * Serialize the resulting inputs assignment
    serialize_inputs,

    -- * Serialize the resulting witness assignment
    serialize_witnesses,

    -- * For a given Snarkl computation, use 'libsnark' to test: (1)

    -- key generation, (2) proof generation, and (3) proof
    -- verification.  Currently assumes 'Snarkl.Toplevel' is loaded in working
    -- directory 'base-of-snarkl-repo'.
    snarkify_comp,
    keygen_comp, -- for benchmarking
    proofgen_comp, -- for benchmarking
    witgen_comp, -- for benchmarking
    r1csgen_comp, -- for benchmarking

    -- * Convenience functions
    Result (..),
    result_of_comp,
    int_of_comp,
    test_comp,
    benchmark_comp,

    -- * Re-exported modules
    module Snarkl.Language,
    module Snarkl.Constraint.Constraints,
    module Snarkl.Constraint.Simplify,
    module Snarkl.Backend.R1CS,
    executeAndWriteArtifacts,
  )
where

import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import Data.Field.Galois (GaloisField, PrimeField)
import qualified Data.Map as Map
import Data.Typeable (Typeable)
import Snarkl.Arkworks (CMD (..), runCMD)
import Snarkl.Backend.R1CS
import Snarkl.Compile
  ( SimplParam,
    TExpPkg (..),
    constrs_of_comp,
    constrs_of_texp,
    r1cs_of_comp,
    r1cs_of_constrs,
    r1cs_of_texp,
    texp_of_comp,
    wit_of_r1cs,
  )
import Snarkl.Constraint.Constraints
import Snarkl.Constraint.Simplify
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Interp (interp)
import Snarkl.Language
import System.Exit (ExitCode (..))
import System.IO
  ( IOMode (WriteMode),
    hFlush,
    hPutStrLn,
    stdout,
    withFile,
  )
import Prelude
import qualified Prelude as P

----------------------------------------------------
--
-- Snarkl.Toplevel Stuff
--
----------------------------------------------------

-- | Using the executable semantics for the 'TExp' language, execute
-- the computation on the provided inputs, returning the 'k' result.
comp_interp ::
  (Typeable ty, GaloisField k) =>
  Comp ty k ->
  [k] ->
  k
comp_interp mf inputs =
  let TExpPkg _ in_vars e = texp_of_comp mf
      input_map = Map.fromList $ zip in_vars inputs
   in case interp input_map e of
        Left err -> failWith err
        Right (_, Nothing) -> failWith $ ErrMsg $ show e ++ " evaluated to bot"
        Right (_, Just v) -> v

-- | For a given R1CS and inputs, serialize the input variable assignment.
serialize_inputs :: (PrimeField k) => [k] -> R1CS k -> String
serialize_inputs inputs r1cs =
  let inputs_assgn = Map.fromList $ zip (r1cs_in_vars r1cs) inputs
   in serialize_assgn inputs_assgn

-- | For a given R1CS and inputs, serialize the witness variable assignment.
serialize_witnesses :: (PrimeField k) => [k] -> R1CS k -> String
serialize_witnesses inputs r1cs =
  let num_in_vars = length $ r1cs_in_vars r1cs
      assgn = wit_of_r1cs inputs r1cs
      inputs_assgn = Map.fromList $ drop num_in_vars $ Map.toAscList assgn
   in serialize_assgn inputs_assgn

------------------------------------------------------
--
-- Convenience functions
--
------------------------------------------------------

-- | The result of compiling and executing a Snarkl computation.
data Result a = Result
  { result_sat :: Bool,
    result_vars :: Int,
    result_constraints :: Int,
    result_result :: a,
    result_r1cs :: String
  }

instance (Show a) => Show (Result a) where
  show (Result the_sat the_vars the_constraints the_result _) =
    "sat = "
      ++ show the_sat
      ++ ", vars = "
      ++ show the_vars
      ++ ", constraints = "
      ++ show the_constraints
      ++ ", result = "
      ++ show the_result

-- | Snarkl.Compile a computation to R1CS, and run it on the provided inputs.
-- Also, interprets the computation using the executable semantics and
-- checks that the results match.
result_of_comp :: (Typeable ty, PrimeField k) => SimplParam -> Comp ty k -> [k] -> Result k
result_of_comp simpl mf inputs =
  execute simpl mf inputs

-- | Same as 'result_of_comp', but specialized to integer arguments
-- and results. Returns just the integer result.
int_of_comp :: (Typeable ty, PrimeField k) => SimplParam -> Comp ty k -> [Int] -> k
int_of_comp simpl mf args =
  result_result $ result_of_comp simpl mf (map fromIntegral args)

-- | Same as 'int_of_comp', but additionally runs resulting R1CS
-- through key generation, proof generation, and verification stages
-- of 'libsnark'.  TODO: This function does duplicate R1CS generation,
-- once for 'libsnark' and a second time for 'int_of_comp'.
test_comp :: (Typeable ty, PrimeField k) => SimplParam -> Comp ty k -> [Int] -> IO (Either ExitCode k)
test_comp simpl mf args =
  do
    exit_code <- snarkify_comp "hspec" simpl mf (map fromIntegral args)
    case exit_code of
      ExitFailure _ -> Prelude.return $ Left exit_code
      ExitSuccess -> Prelude.return $ Right (int_of_comp simpl mf args)

--------------------------------------------------
--
-- Internal Functions
--
--------------------------------------------------

-- | (1) Snarkl.Compile to R1CS.
--   (2) Generate a satisfying assignment, 'w'.
--   (3) Check whether 'w' satisfies the constraint system produced in (1).
--   (4) Check whether the R1CS result matches the interpreter result.
--   (5) Return the 'Result'.
execute :: (Typeable ty, PrimeField k) => SimplParam -> Comp ty k -> [k] -> Result k
execute simpl mf inputs =
  let TExpPkg nv in_vars e = texp_of_comp mf
      r1cs = r1cs_of_texp simpl (TExpPkg nv in_vars e)
      r1cs_string = serialize_r1cs r1cs
      [out_var] = r1cs_out_vars r1cs
      wit = wit_of_r1cs inputs r1cs
      out = case Map.lookup out_var wit of
        Nothing ->
          failWith $
            ErrMsg
              ( "output variable "
                  ++ show out_var
                  ++ "not mapped, in\n  "
                  ++ show wit
              )
        Just out_val -> out_val
      -- Interpret the program using the executable semantics and
      -- the input assignment (a subset of 'wit').
      -- Output the return value of 'e'.
      out_interp = comp_interp mf inputs
      result =
        ( if out_interp == out
            then sat_r1cs wit r1cs
            else
              failWith $
                ErrMsg $
                  "interpreter result "
                    ++ show out_interp
                    ++ " differs from actual result "
                    ++ show out
        )
      nw = r1cs_num_vars r1cs
      ng = num_constraints r1cs
   in Result result nw ng out r1cs_string

-- | 'execute' computation, reporting error if result doesn't match
-- the return value provided by the caller. Also, serializes the
-- resulting 'R1CS'.
benchmark_comp :: (Typeable ty, PrimeField k) => (SimplParam, Comp ty k, [k], k) -> IO ()
benchmark_comp (simpl, prog, inputs, res) =
  let print_ln = print_ln_to_file stdout
      print_ln_to_file h s = (P.>>) (hPutStrLn h s) (hFlush h)
      print_to_file s =
        withFile "test_cs_in.ppzksnark" WriteMode (flip print_ln_to_file s)
   in case execute simpl prog inputs of
        r@(Result True _ _ res' r1cs_string) ->
          if res == res'
            then do
              print_to_file r1cs_string
              print_ln $ show r
            else
              print_ln $
                show $
                  "error: results don't match: "
                    ++ "expected "
                    ++ show res
                    ++ " but got "
                    ++ show res'
        Result False _ _ _ _ ->
          print_ln $ "error: witness failed to satisfy constraints"

--------------------------------------------------------------------------------

executeAndWriteArtifacts :: (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> [k] -> IO ()
executeAndWriteArtifacts name simpl mf inputs =
  let TExpPkg nv in_vars e = texp_of_comp mf
      r1cs = r1cs_of_texp simpl (TExpPkg nv in_vars e)
      [out_var] = r1cs_out_vars r1cs
      wit = wit_of_r1cs inputs r1cs
      out = case Map.lookup out_var wit of
        Nothing ->
          failWith $
            ErrMsg
              ( "output variable "
                  ++ show out_var
                  ++ "not mapped, in\n  "
                  ++ show wit
              )
        Just out_val -> out_val
   in -- Interpret the program using the executable semantics and
      -- the input assignment (a subset of 'wit').
      -- Output the return value of 'e'.
      do
        let out_interp = comp_interp mf inputs
        unless (out == out_interp Prelude.&& sat_r1cs wit r1cs) $
          failWith $
            ErrMsg $
              "interpreter result "
                ++ show out_interp
                ++ " differs from actual result "
                ++ show out
        let r1csFP = name <> "-r1cs.jsonl"
        LBS.writeFile r1csFP (serializeR1CSAsJson r1cs)
        putStrLn $ "Wrote R1CS to file " <> r1csFP
        let witnessFP = name <> "-witness.jsonl"
        LBS.writeFile witnessFP (serializeWitnessAsJson r1cs wit)
        putStrLn $ "Wrote witness to file " <> witnessFP
        let inputsFP = name <> "-inputs.jsonl"
        LBS.writeFile inputsFP (serializeInputsAsJson r1cs inputs)
        putStrLn $ "Wrote inputs to file " <> inputsFP

--------------------------------------------------------------------------------

-- |                       *** WARNING ***
-- This function creates/overwrites files prefixed with 'filePrefix',
-- within the scripts/ subdirectory. 'snarkify_comp' also
-- assumes that it's run in working directory 'base-of-snarkl-repo'.
snarkify_comp :: forall ty k. (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> [k] -> IO ExitCode
snarkify_comp filePrefix simpl c inputs =
  runCMD $ RunR1CS "scripts" filePrefix simpl c inputs

keygen_comp :: (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> [k] -> IO ExitCode
keygen_comp filePrefix simpl c _ = runCMD $ CreateTrustedSetup "scripts" filePrefix simpl c

proofgen_comp :: (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> [k] -> IO ExitCode
proofgen_comp filePrefix simpl c inputs = runCMD $ CreateProof "scripts" filePrefix simpl c inputs

r1csgen_comp :: (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> IO ()
r1csgen_comp filePrefix simpl c =
  do
    let r1cs = r1cs_of_comp simpl c
        r1cs_file = "scripts/" ++ filePrefix ++ "-r1cs.jsonl"
    LBS.writeFile r1cs_file (serializeR1CSAsJson r1cs)

witgen_comp :: (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> [k] -> IO ()
witgen_comp filePrefix simpl c inputs = do
  let r1cs = r1cs_of_comp simpl c
      wit = wit_of_r1cs inputs r1cs
      r1cs_file = "scripts/" ++ filePrefix ++ "-r1cs.jsonl"
      wits_file = "scripts/" ++ filePrefix ++ "-witness.jsonl"
  LBS.writeFile r1cs_file (serializeR1CSAsJson r1cs)
  LBS.writeFile wits_file (serializeWitnessAsJson r1cs wit)
