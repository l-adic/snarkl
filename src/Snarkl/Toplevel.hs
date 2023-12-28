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

import Control.Lens (view)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import Data.Field.Galois (GaloisField, PrimeField)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable (Typeable)
import Prettyprinter (Pretty (pretty))
import Snarkl.Backend.R1CS
import Snarkl.Common (Var (Var))
import Snarkl.Compile
  ( SimplParam,
    constraints_of_texp,
    r1cs_of_constraints,
    _Var,
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
import System.Process
  ( createProcess,
    shell,
    waitForProcess,
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

------------------------------------------------------
--
-- 'TExp'
--
------------------------------------------------------

-- | The result of desugaring a Snarkl computation.
data TExpPkg ty k = TExpPkg
  { -- | The number of free variables in the computation.
    comp_num_vars :: Int,
    -- | The variables marked as inputs.
    comp_input_vars :: [Variable],
    -- | The resulting 'TExp'.
    comp_texp :: TExp ty k
  }
  deriving (Show)

instance (Typeable ty, Pretty k) => Pretty (TExpPkg ty k) where
  pretty (TExpPkg _ _ e) = pretty e

deriving instance (Eq (TExp ty k)) => Eq (TExpPkg ty k)

-- | Desugar a 'Comp'utation to a pair of:
--   the total number of vars,
--   the input vars,
--   the 'TExp'.
texp_of_comp ::
  Comp ty k ->
  TExpPkg ty k
texp_of_comp mf =
  case run mf of
    Left err -> failWith err
    Right (e, rho) ->
      let nv = next_variable rho
          in_vars = sort $ input_vars rho
       in TExpPkg nv in_vars e
  where
    run :: State (Env k) a -> CompResult (Env k) a
    run mf0 =
      runState
        mf0
        ( Env
            (fromInteger 0)
            (fromInteger 0)
            []
            Map.empty
            Map.empty
        )

------------------------------------------------------
--
-- Constraint generation
--
------------------------------------------------------

-- | Snarkl.Compile 'TExp's to constraint systems. Re-exported from 'Snarkl.Compile.Snarkl.Compile'.
constrs_of_texp ::
  (Typeable ty, GaloisField k) =>
  TExpPkg ty k ->
  ConstraintSystem k
constrs_of_texp (TExpPkg out in_vars e) = constraints_of_texp (Var out) (map (view _Var) in_vars) e

-- | Snarkl.Compile Snarkl computations to constraint systems.
constrs_of_comp ::
  (Typeable ty, GaloisField k) =>
  Comp ty k ->
  ConstraintSystem k
constrs_of_comp = constrs_of_texp . texp_of_comp

------------------------------------------------------
--
-- R1CS
--
------------------------------------------------------

-- | Snarkl.Compile constraint systems to 'R1CS'. Re-exported from 'Constraints.hs'.
r1cs_of_constrs ::
  (GaloisField a) =>
  SimplParam ->
  -- | Constraints
  ConstraintSystem a ->
  R1CS a
r1cs_of_constrs = r1cs_of_constraints

-- | Snarkl.Compile 'TExp's to 'R1CS'.
r1cs_of_texp ::
  (Typeable ty, GaloisField k) =>
  SimplParam ->
  TExpPkg ty k ->
  R1CS k
r1cs_of_texp simpl = (r1cs_of_constrs simpl) . constrs_of_texp

-- | Snarkl.Compile Snarkl computations to 'R1CS'.
r1cs_of_comp ::
  (Typeable ty, GaloisField k) =>
  SimplParam ->
  Comp ty k ->
  R1CS k
r1cs_of_comp simpl = (r1cs_of_constrs simpl) . constrs_of_comp

-- | For a given R1CS and inputs, calculate a satisfying assignment.
wit_of_r1cs :: [k] -> R1CS k -> Map Var k
wit_of_r1cs inputs r1cs =
  let in_vars = r1cs_in_vars r1cs
      f = r1cs_gen_witness r1cs . Map.fromList
   in case length in_vars /= length inputs of
        True ->
          failWith $
            ErrMsg
              ( "expected "
                  ++ show (length in_vars)
                  ++ " input(s)"
                  ++ " but got "
                  ++ show (length inputs)
                  ++ " input(s)"
              )
        False ->
          f (zip in_vars inputs)

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
-- Libsnark hooks
--
------------------------------------------------------

-- |                       *** WARNING ***
-- This function creates/overwrites files prefixed with 'filePrefix',
-- within the scripts/ subdirectory. 'snarkify_comp' also
-- assumes that it's run in working directory 'base-of-snarkl-repo'.
-- arkify_comp :: forall ty k. (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> [k] -> IO ExitCode
-- arkify_comp filePrefix simpl c inputs =
--  do
--    let r1cs = r1cs_of_comp simpl c
--        r1cs_file = filePrefix ++ ".r1cs"
--        inputs_file = filePrefix ++ ".inputs"
--        wits_file = filePrefix ++ ".wits"
--        run_r1cs = "./run-r1cs.sh"
--
--    withFile
--      ("scripts/" ++ r1cs_file)
--      WriteMode
--      ( \h ->
--          hPutStrLn h $ serialize_r1cs r1cs
--      )
--
--    withFile
--      ("scripts/" ++ inputs_file)
--      WriteMode
--      ( \h ->
--          hPutStr h $ serialize_inputs inputs r1cs
--      )
--
--    withFile
--      ("scripts/" ++ wits_file)
--      WriteMode
--      ( \h ->
--          hPutStr h $ serialize_witnesses inputs r1cs
--      )
--
--    (_, _, _, hdl) <-
--      createProcess
--        (proc run_r1cs [r1cs_file, inputs_file, wits_file])
--          { cwd = Just "scripts"
--          }
--
--    waitForProcess hdl

-- Like snarkify_comp, but only generate witnesses and keys
-- Serializes r1cs, inputs, and witnesses to files.
-- keygen_comp :: (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> [k] -> IO ExitCode
-- keygen_comp filePrefix simpl c inputs =
--  do
--    let r1cs = r1cs_of_comp simpl c
--        r1cs_file = filePrefix ++ ".r1cs"
--        inputs_file = filePrefix ++ ".inputs"
--        wits_file = filePrefix ++ ".wits"
--        run_r1cs = "./run-keygen.sh"
--
--    withFile
--      ("scripts/" ++ r1cs_file)
--      WriteMode
--      ( \h ->
--          hPutStrLn h $ serialize_r1cs r1cs
--      )
--
--    withFile
--      ("scripts/" ++ inputs_file)
--      WriteMode
--      ( \h ->
--          hPutStr h $ serialize_inputs inputs r1cs
--      )
--
--    withFile
--      ("scripts/" ++ wits_file)
--      WriteMode
--      ( \h ->
--          hPutStr h $ serialize_witnesses inputs r1cs
--      )
--
--    (_, _, _, hdl) <-
--      createProcess
--        (proc run_r1cs [r1cs_file, inputs_file, wits_file])
--          { cwd = Just "scripts"
--          }
--
--    waitForProcess hdl

-- Like snarkify_comp, but only generate keys and proof
-- (no verification)
-- Serializes r1cs, inputs, witnesses.
-- proofgen_comp :: (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> [k] -> IO ExitCode
-- proofgen_comp filePrefix simpl c inputs =
--  do
--    let r1cs = r1cs_of_comp simpl c
--        r1cs_file = filePrefix ++ ".r1cs"
--        inputs_file = filePrefix ++ ".inputs"
--        wits_file = filePrefix ++ ".wits"
--        run_r1cs = "./run-proofgen.sh"
--
--    withFile
--      ("scripts/" ++ r1cs_file)
--      WriteMode
--      ( \h ->
--          hPutStrLn h $ serialize_r1cs r1cs
--      )
--
--    withFile
--      ("scripts/" ++ inputs_file)
--      WriteMode
--      ( \h ->
--          hPutStr h $ serialize_inputs inputs r1cs
--      )
--
--    withFile
--      ("scripts/" ++ wits_file)
--      WriteMode
--      ( \h ->
--          hPutStr h $ serialize_witnesses inputs r1cs
--      )
--
--    (_, _, _, hdl) <-
--      createProcess
--        (proc run_r1cs [r1cs_file, inputs_file, wits_file])
--          { cwd = Just "scripts"
--          }
--
--    waitForProcess hdl

-- Like snarkify_comp, but only generate and serialize the r1cs
-- r1csgen_comp :: (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> IO ()
-- r1csgen_comp filePrefix simpl c =
--  do
--    let r1cs = r1cs_of_comp simpl c
--        r1cs_file = filePrefix ++ ".r1cs"
--
--    withFile
--      ("scripts/" ++ r1cs_file)
--      WriteMode
--      ( \h ->
--          hPutStrLn h $ serialize_r1cs r1cs
--      )

-- Like snarkify_comp, but only generate the witness
-- (no key generation or proof)
-- Serializes r1cs, inputs, and witnesses to files.
-- witgen_comp :: (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> [k] -> IO ()
-- witgen_comp filePrefix simpl c inputs =
--  do
--    let r1cs = r1cs_of_comp simpl c
--        r1cs_file = filePrefix ++ ".r1cs"
--        inputs_file = filePrefix ++ ".inputs"
--        wits_file = filePrefix ++ ".wits"
--
--    withFile
--      ("scripts/" ++ r1cs_file)
--      WriteMode
--      ( \h ->
--          hPutStrLn h $ serialize_r1cs r1cs
--      )
--
--    withFile
--      ("scripts/" ++ inputs_file)
--      WriteMode
--      ( \h ->
--          hPutStr h $ serialize_inputs inputs r1cs
--      )
--
--    withFile
--      ("scripts/" ++ wits_file)
--      WriteMode
--      ( \h ->
--          hPutStr h $ serialize_witnesses inputs r1cs
--      )

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
  do
    let r1cs = r1cs_of_comp simpl c
        wit = wit_of_r1cs inputs r1cs
        r1cs_file = "scripts/" ++ filePrefix ++ "-r1cs.jsonl"
        inputs_file = "scripts/" ++ filePrefix <> "-inputs.jsonl"
        wits_file = "scripts/" ++ filePrefix ++ "-witness.jsonl"
        -- run_r1cs = "echo $PATH"
        run_r1cs =
          mconcat
            [ "arkworks-bridge run-r1cs ",
              "--inputs " <> inputs_file <> " ",
              "--witness " <> wits_file <> " ",
              "--r1cs " <> r1cs_file
            ]
    LBS.writeFile r1cs_file (serializeR1CSAsJson r1cs)
    LBS.writeFile wits_file (serializeWitnessAsJson r1cs wit)
    LBS.writeFile inputs_file (serializeInputsAsJson r1cs inputs)

    (_, _, _, hdl) <- createProcess $ shell run_r1cs

    waitForProcess hdl

keygen_comp :: (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> [k] -> IO ExitCode
keygen_comp filePrefix simpl c _ = do
  let r1cs = r1cs_of_comp simpl c
      r1cs_file = "scripts/" ++ filePrefix ++ "-r1cs.jsonl"
      pk_file = "scripts/" ++ filePrefix <> "-pk"
      vk_file = "scripts/" ++ filePrefix ++ "-vk"
      -- run_r1cs = "echo $PATH"
      run_r1cs =
        mconcat
          [ "arkworks-bridge create-trusted-setup ",
            "--r1cs " <> r1cs_file <> " ",
            "--verifying-key " <> vk_file <> " ",
            "--proving-key " <> pk_file
          ]
  LBS.writeFile r1cs_file (serializeR1CSAsJson r1cs)

  (_, _, _, hdl) <- createProcess $ shell run_r1cs

  waitForProcess hdl

proofgen_comp :: (Typeable ty, PrimeField k) => String -> SimplParam -> Comp ty k -> [k] -> IO ExitCode
proofgen_comp filePrefix simpl c inputs = do
  let r1cs = r1cs_of_comp simpl c
      wit = wit_of_r1cs inputs r1cs
      r1cs_file = "scripts/" ++ filePrefix ++ "-r1cs.jsonl"
      pk_file = "scripts/" ++ filePrefix <> "-pk"
      wits_file = "scripts/" ++ filePrefix ++ "-witness.jsonl"
      proof_file = "scripts/" ++ filePrefix ++ "-proof"
      -- run_r1cs = "echo $PATH"
      run_r1cs =
        mconcat
          [ "arkworks-bridge create-proof ",
            "--witness " <> wits_file <> " ",
            "--r1cs " <> r1cs_file <> " ",
            "--proof " <> proof_file <> " ",
            "--proving-key " <> pk_file
          ]
  LBS.writeFile r1cs_file (serializeR1CSAsJson r1cs)
  LBS.writeFile wits_file (serializeWitnessAsJson r1cs wit)

  (_, _, _, hdl) <- createProcess $ shell run_r1cs

  waitForProcess hdl

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
