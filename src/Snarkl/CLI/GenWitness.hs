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
import qualified Data.ByteString.Lazy as LBS
import Data.Field.Galois (PrimeField (fromP))
import Data.Foldable (Foldable (toList))
import qualified Data.Map as Map
import qualified Data.String.Conversions as CS
import Data.Typeable (Typeable)
import Options.Applicative (Parser, eitherReader, help, long, option, strOption)
import Snarkl.Backend.R1CS
import Snarkl.Constraint (ConstraintSystem (cs_out_vars), SimplifiedConstraintSystem (unSimplifiedConstraintSystem), constraintSystemToHeader, mkConstraintsFilePath, parseConstraintSystem)
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Language
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
            <> help "The directory where the inputs.jsonl file is located"
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
          <> help "the directory where the constrains.jsonl file is located"
      )
    <*> strOption
      ( long "r1cs-input-dir"
          <> help "the directory where the r1cs.jsonl file is located"
      )
    <*> inputOptsParser
    <*> strOption
      ( long "witness-output-dir"
          <> help "the directory to write the witness.jsonl file"
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
  let csFP = mkConstraintsFilePath constraintsInput name
  constraintsBS <- LBS.readFile csFP
  let constraints = either error id $ parseConstraintSystem constraintsBS
      [out_var] = cs_out_vars (unSimplifiedConstraintSystem constraints)
  -- parse the inputs, either from cli or from file
  is <- case inputs of
    Explicit is -> pure $ map fromInteger is
    FromFile fp -> do
      inputsBS <- LBS.readFile fp
      either error pure $ parseInputs inputsBS
  let out_interp = comp_interp comp is
      witness = wit_of_cs is constraints
      out = case Map.lookup out_var witness of
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
  let r1csFP = mkR1CSFilePath r1csInput name
  r1csBS <- LBS.readFile r1csFP
  let r1cs = either error id $ deserializeR1CS r1csBS
  unless (sat_r1cs witness r1cs) $
    failWith $
      ErrMsg $
        "witness\n  "
          ++ CS.cs (A.encode $ toList (fromP <$> witness))
          ++ "\nfailed to satisfy R1CS\n  "
          ++ CS.cs (A.encode $ r1cs_clauses r1cs)
  let witnessFP = mkWitnessFilePath witnessOutput name
  LBS.writeFile witnessFP (serializeWitnessAsJson (constraintSystemToHeader constraints) witness)
  putStrLn $ "Wrote witness to file " <> witnessOutput
