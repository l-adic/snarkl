{-# LANGUAGE RecordWildCards #-}

module Snarkl.CLI.RunAll where

import Control.Monad (unless)
import qualified Data.Aeson as A
import Data.Field.Galois (PrimeField)
import Data.JSONLines (FromJSONLines (..), NoHeader (..), ToJSONLines (..), WithHeader (..))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.String.Conversions as CS
import Data.Typeable (Typeable)
import Options.Applicative (Parser, help, long, showDefault, strOption, value)
import Snarkl.Backend.R1CS (R1CS (r1cs_clauses, r1cs_out_vars), Witness (Witness, witness_assgn), r1csHeader, sat_r1cs)
import Snarkl.CLI.Common (mkR1CSFilePath, mkWitnessFilePath, readFileLines, writeFileWithDir)
import Snarkl.CLI.Compile (OptimizeOpts (removeUnreachable, simplify), optimizeOptsParser)
import Snarkl.CLI.GenWitness (InputOpts (Explicit, FromFile), inputOptsParser)
import Snarkl.Common (Assgn (Assgn), FieldElem (FieldElem, unFieldElem))
import Snarkl.Compile (SimplParam (RemoveUnreachable, Simplify), TExpPkg (TExpPkg), compileCompToTexp, compileTExpToR1CS)
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Language (Comp)
import Snarkl.Toplevel (comp_interp, wit_of_cs)

data RunAllOpts = RunAllOpts
  { r1csOutput :: FilePath,
    optimizeOpts :: OptimizeOpts,
    inputs :: InputOpts,
    witnessOutput :: FilePath
  }

runAllOptsParser :: Parser RunAllOpts
runAllOptsParser =
  RunAllOpts
    <$> strOption
      ( long "r1cs-output-dir"
          <> help "the directory to write the r1cs.jsonl artifact"
          <> value "./snarkl-output"
          <> showDefault
      )
    <*> optimizeOptsParser
    <*> inputOptsParser
    <*> strOption
      ( long "witness-output-dir"
          <> help "the directory to write the witness.jsonl artifact"
          <> value "./snarkl-output"
          <> showDefault
      )

runAll ::
  forall ty k.
  (Typeable ty) =>
  (PrimeField k) =>
  RunAllOpts ->
  String ->
  Comp ty k ->
  IO ()
runAll RunAllOpts {..} name comp = do
  let simpl =
        catMaybes
          [ if simplify optimizeOpts then Just Simplify else Nothing,
            if removeUnreachable optimizeOpts then Just RemoveUnreachable else Nothing
          ]
      TExpPkg nv in_vars e = compileCompToTexp comp
      (r1cs, cs) = compileTExpToR1CS simpl (TExpPkg nv in_vars e)
  let [out_var] = r1cs_out_vars r1cs
  -- parse the inputs, either from cli or from file
  is <- case inputs of
    Explicit is -> pure $ map fromInteger is
    FromFile fp -> do
      eInput <- fromJSONLines <$> readFileLines fp
      NoHeader input <- either (failWith . ErrMsg) pure eInput
      pure $ map unFieldElem input
  let out_interp = comp_interp comp is
      witness@(Witness {witness_assgn = Assgn m}) = wit_of_cs is cs
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
  let witnessAssgn@(Assgn assgn) = witness_assgn witness
  unless (sat_r1cs witness r1cs) $
    failWith $
      ErrMsg $
        "witness\n  "
          ++ CS.cs (A.encode witnessAssgn)
          ++ "\nfailed to satisfy R1CS\n  "
          ++ CS.cs (A.encode $ r1cs_clauses r1cs)
  let r1csFP = mkR1CSFilePath r1csOutput name
  writeFileWithDir r1csFP . toJSONLines $
    WithHeader (r1csHeader r1cs) (r1cs_clauses r1cs)
  putStrLn $ "Wrote R1CS to " <> r1csFP
  let witnessFP = mkWitnessFilePath witnessOutput name
  writeFileWithDir witnessFP . toJSONLines $
    WithHeader (r1csHeader r1cs) (Map.toList $ fmap FieldElem assgn)
  putStrLn $ "Wrote witness to " <> witnessFP
