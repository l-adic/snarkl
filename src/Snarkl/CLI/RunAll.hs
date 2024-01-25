{-# LANGUAGE RecordWildCards #-}

module Snarkl.CLI.RunAll where

import Control.Monad (unless)
import qualified Data.Aeson as A
import Data.Field.Galois (PrimeField)
import Data.JSONLines (FromJSONLines (..), ToJSONLines (..))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.String.Conversions as CS
import Data.Typeable (Typeable)
import Options.Applicative (Parser, help, long, showDefault, strOption, value)
import Snarkl.AST (Comp)
import Snarkl.Backend.R1CS (R1CS (r1cs_clauses, r1cs_out_vars), Witness (Witness, witness_assgn), sat_r1cs)
import Snarkl.CLI.Common (mkAssignmentsFilePath, mkR1CSFilePath, mkWitnessFilePath, readFileLines, writeFileWithDir)
import Snarkl.CLI.Compile (OptimizeOpts (removeUnreachable, simplify), optimizeOptsParser)
import Snarkl.Common (Assgn (Assgn), splitInputAssignments)
import Snarkl.Compile (SimplParam (RemoveUnreachable, Simplify), compileCompToTexp, compileTExpToR1CS)
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Toplevel (comp_interp, wit_of_cs)

data RunAllOpts = RunAllOpts
  { r1csOutput :: FilePath,
    optimizeOpts :: OptimizeOpts,
    assignments :: FilePath,
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
    <*> strOption
      ( long "assignments-dir"
          <> help "the directory where the assignments.jsonl artifact is located"
      )
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
      texpPkg = compileCompToTexp comp
      (r1cs, cs, _) = compileTExpToR1CS simpl texpPkg
  let [out_var] = r1cs_out_vars r1cs
  -- parse the inputs, either from cli or from file
  (pubInputs, privInputs, outputs) <- do
    let assignmentsFP = mkAssignmentsFilePath assignments name
    eInput <- fromJSONLines <$> readFileLines assignmentsFP
    let inputAssignments = either (failWith . ErrMsg) id eInput
    pure $ splitInputAssignments inputAssignments
  let mOut = case outputs of
        [(v, k)] ->
          if v /= out_var
            then failWith $ ErrMsg $ "derived output variable " <> show out_var <> " does not match provided output variable " <> show v
            else Just (v, k)
        [] -> Nothing
        _ -> failWith $ ErrMsg $ "expected one output variable, got " <> show outputs
      initAssignments = Map.mapKeys snd privInputs <> maybe mempty (uncurry Map.singleton) mOut
  let witness@(Witness {witness_assgn = Assgn m}) = wit_of_cs pubInputs initAssignments cs
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
      out_interp = comp_interp comp pubInputs (Map.mapKeys fst privInputs)
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
          ++ "\nfailed to satisfy R1CS\n  "
          ++ CS.cs (A.encode $ r1cs_clauses r1cs)
  let r1csFP = mkR1CSFilePath r1csOutput name
  writeFileWithDir r1csFP $ toJSONLines r1cs
  putStrLn $ "Wrote R1CS to " <> r1csFP
  let witnessFP = mkWitnessFilePath witnessOutput name
  writeFileWithDir witnessFP $ toJSONLines witness
  putStrLn $ "Wrote witness to " <> witnessFP
