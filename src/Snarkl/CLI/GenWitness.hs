{-# LANGUAGE RecordWildCards #-}

module Snarkl.CLI.GenWitness
  ( GenWitnessOpts,
    genWitnessOptsParser,
    genWitness,
    InputOpts (..),
    inputOptsParser,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import qualified Data.Aeson as A
import Data.Field.Galois (PrimeField)
import Data.JSONLines (FromJSONLines (fromJSONLines), ToJSONLines (toJSONLines))
import Data.List (sortOn)
import qualified Data.Map as Map
import qualified Data.String.Conversions as CS
import Data.Typeable (Typeable)
import Options.Applicative (Parser, eitherReader, help, long, option, showDefault, strOption, value)
import Snarkl.Backend.R1CS
import Snarkl.CLI.Common (mkConstraintsFilePath, mkR1CSFilePath, mkWitnessFilePath, readFileLines, writeFileWithDir)
import Snarkl.Common (Assgn (Assgn))
import Snarkl.Constraint (ConstraintSystem (..), SimplifiedConstraintSystem (unSimplifiedConstraintSystem))
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Language (Comp)
import Snarkl.Toplevel (comp_interp, wit_of_cs)
import Text.Read (readEither)

data InputOpts
  = Explicit [Integer]
  | FromFile FilePath

inputOptsParser :: Parser InputOpts
inputOptsParser =
  Explicit
    <$> option
      (eitherReader readEither)
      ( long "inputs"
          <> help "an input value"
      )
    <|> FromFile
      <$> strOption
        ( long "inputs-dir"
            <> help "The directory where the inputs.jsonl artifact is located"
        )

data GenWitnessOpts = GenWitnessOpts
  { constraintsInput :: FilePath,
    r1csInput :: FilePath,
    inputs :: InputOpts,
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
    <*> inputOptsParser
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
  is <- case inputs of
    Explicit is -> pure $ map fromInteger is
    FromFile fp -> do
      eInput <- fromJSONLines <$> readFileLines fp
      let Assgn input = either (failWith . ErrMsg) id eInput
      pure . map snd . sortOn fst $ Map.toList input
  let out_interp = comp_interp comp is
      witness@(Witness {witness_assgn = Assgn m}) = wit_of_cs is constraints
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
