{-# LANGUAGE RecordWildCards #-}

module Snarkl.CLI.GenWitness
  ( GenWitnessOpts,
    genWitnessOptsParser,
    genWitness,
  )
where

import Control.Monad (unless)
import qualified Data.Aeson as A
import Data.Field.Galois (PrimeField)
import Data.JSONLines (FromJSONLines (fromJSONLines), ToJSONLines (toJSONLines))
import qualified Data.Map as Map
import qualified Data.String.Conversions as CS
import Data.Typeable (Typeable)
import Options.Applicative (Parser, help, long, showDefault, strOption, value)
import Snarkl.AST (Comp)
import Snarkl.Backend.R1CS
  ( R1CS (r1cs_clauses),
    Witness (Witness, witness_assgn),
    sat_r1cs,
  )
import Snarkl.CLI.Common (mkAssignmentsFilePath, mkConstraintsFilePath, mkR1CSFilePath, mkWitnessFilePath, readFileLines, writeFileWithDir)
import Snarkl.Common (Assgn (Assgn), splitInputAssignments)
import Snarkl.Constraint (ConstraintSystem (..), SimplifiedConstraintSystem (unSimplifiedConstraintSystem))
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Toplevel (comp_interp, wit_of_cs)

data GenWitnessOpts = GenWitnessOpts
  { constraintsInput :: FilePath,
    r1csInput :: FilePath,
    assignments :: FilePath,
    witnessOutput :: FilePath
  }

genWitnessOptsParser :: Parser GenWitnessOpts
genWitnessOptsParser =
  GenWitnessOpts
    <$> strOption
      ( long "constraints-input-dir"
          <> help "the directory where the constraints.jsonl artifact is located"
          <> value "./snarkl-output"
          <> showDefault
      )
    <*> strOption
      ( long "r1cs-input-dir"
          <> help "the directory where the r1cs.jsonl file is located"
          <> value "./snarkl-output"
          <> showDefault
      )
    <*> strOption
      ( long "assignments-dir"
          <> help "the directory where the assignments.jsonl file is located"
      )
    <*> strOption
      ( long "witness-output-dir"
          <> help "the directory to write the witness.jsonl file"
          <> value "./snarkl-output"
          <> showDefault
      )

genWitness ::
  forall ty k.
  (Typeable ty) =>
  (PrimeField k) =>
  GenWitnessOpts ->
  String ->
  Comp ty k ->
  IO ()
genWitness GenWitnessOpts {..} name comp = do
  --  parse the constraints file
  constraints <- do
    let csFP = mkConstraintsFilePath constraintsInput name
    eConstraints <- fromJSONLines <$> readFileLines csFP
    either (failWith . ErrMsg) pure eConstraints
  let [out_var] = cs_out_vars (unSimplifiedConstraintSystem constraints)
  -- parse the inputs, either from cli or from file
  (pubInputs, privInputs) <- do
    let assignmentsFP = mkAssignmentsFilePath assignments name
    eInput <- fromJSONLines <$> readFileLines assignmentsFP
    let inputAssignments = either (failWith . ErrMsg) id eInput
    pure $ splitInputAssignments inputAssignments
  let out_interp = comp_interp comp pubInputs (Map.mapKeys fst privInputs)
      witness@(Witness {witness_assgn = Assgn m}) = wit_of_cs pubInputs (Map.mapKeys snd privInputs) constraints
      out = case Map.lookup out_var m of
        Nothing ->
          failWith $
            ErrMsg
              ( "output variable "
                  ++ show out_var
                  ++ "not mapped, in\n  "
                  ++ show witness
              )
        Just out_val -> out_val
  unless (out_interp == out) $
    failWith $
      ErrMsg $
        "interpreter result "
          ++ show out_interp
          ++ " differs from actual result "
          ++ show out
  r1cs <- do
    let r1csFP = mkR1CSFilePath r1csInput name
    eConstraints <- fromJSONLines <$> readFileLines r1csFP
    either (failWith . ErrMsg) pure eConstraints
  unless (sat_r1cs witness r1cs) $
    failWith $
      ErrMsg $
        "witness failed to satisfy R1CS\n  "
          ++ CS.cs (A.encode $ r1cs_clauses r1cs)
  let witnessFP = mkWitnessFilePath witnessOutput name
  writeFileWithDir witnessFP $ toJSONLines witness
  putStrLn $ "Wrote witness to " <> witnessFP
