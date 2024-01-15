{-# LANGUAGE RecordWildCards #-}

module Snarkl.CLI.RunAll where

import Control.Monad (unless)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Field.Galois (PrimeField (fromP))
import Data.Foldable (Foldable (toList))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.String.Conversions as CS
import Data.Typeable (Typeable)
import Options.Applicative (Parser, help, long, strOption)
import Snarkl.Backend.R1CS (R1CS (r1cs_clauses, r1cs_out_vars), mkR1CSFilePath, mkWitnessFilePath, parseInputs, r1csToHeader, sat_r1cs, serializeR1CSAsJson, serializeWitnessAsJson)
import Snarkl.CLI.Compile (OptimizeOpts (removeUnreachable, simplify), optimizeOptsParser)
import Snarkl.CLI.GenWitness (InputOpts (Explicit, FromFile), inputOptsParser)
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
          <> help "the directory to write the r1cs artifact"
      )
    <*> optimizeOptsParser
    <*> inputOptsParser
    <*> strOption
      ( long "witness-output-dir"
          <> help "the directory to write the witness.jsonl file"
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
      inputsBS <- LBS.readFile fp
      either error pure $ parseInputs inputsBS
  let out_interp = comp_interp comp is
      witness = wit_of_cs is cs
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
  unless (sat_r1cs witness r1cs) $
    failWith $
      ErrMsg $
        "witness\n  "
          ++ CS.cs (A.encode $ toList (fromP <$> witness))
          ++ "\nfailed to satisfy R1CS\n  "
          ++ CS.cs (A.encode $ r1cs_clauses r1cs)
  let r1csFP = mkR1CSFilePath r1csOutput name
  LBS.writeFile r1csFP (serializeR1CSAsJson r1cs)
  putStrLn $ "Wrote R1CS to file " <> r1csOutput
  let witnessFP = mkWitnessFilePath witnessOutput name
  LBS.writeFile witnessFP (serializeWitnessAsJson (r1csToHeader r1cs) witness)
  putStrLn $ "Wrote witness to file " <> witnessOutput
