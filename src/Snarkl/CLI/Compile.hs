{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Snarkl.CLI.Compile
  ( CompileOpts (CompileOpts),
    compile,
    compileOptsParser,
    OptimizeOpts (..),
    optimizeOptsParser,
  )
where

import Control.Monad (unless)
import qualified Data.Aeson as A
import Data.Field.Galois (PrimeField)
import Data.Foldable (Foldable (toList))
import Data.JSONLines (ToJSONLines (toJSONLines), WithHeader (..))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Options.Applicative (CommandFields, Mod, Parser, command, execParser, fullDesc, header, help, helper, info, long, progDesc, showDefault, strOption, subparser, switch, value, (<**>))
import Snarkl.AST (Comp, InputVariable (PrivateInput))
import Snarkl.CLI.Common (mkConstraintsFilePath, mkInputsFilePath, mkR1CSFilePath, writeFileWithDir)
import Snarkl.Common (InputVar (OutputVar, PrivateInputVar, PublicInputVar))
import Snarkl.Compile
  ( SimplParam (RemoveUnreachable, Simplify),
    TExpPkg (TExpPkg),
    compileCompToTexp,
    compileTExpToR1CS,
  )
import Snarkl.Constraint (ConstraintSystem (cs_constraints, cs_out_vars, cs_public_in_vars), SimplifiedConstraintSystem (unSimplifiedConstraintSystem))
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Toplevel (comp_interp, wit_of_cs)
import Text.PrettyPrint.Leijen.Text (Pretty (pretty))

data OptimizeOpts = OptimizeOpts
  { simplify :: Bool,
    removeUnreachable :: Bool
  }

optimizeOptsParser :: Parser OptimizeOpts
optimizeOptsParser =
  OptimizeOpts
    <$> switch
      ( long "simplify"
          <> help "run the constraint simplifier"
      )
    <*> switch
      ( long "remove-unreachable"
          <> help "detect and remove variables not contributing to the output"
      )

data CompileOpts = CompileOpts
  { optimizeOpts :: OptimizeOpts,
    r1csOutput :: FilePath,
    constraintsOutput :: FilePath,
    inputsOutput :: FilePath
  }

compileOptsParser :: Parser CompileOpts
compileOptsParser =
  CompileOpts
    <$> optimizeOptsParser
    <*> strOption
      ( long "r1cs-output-dir"
          <> help "the directory to write the r1cs.jsonl artifact"
          <> value "./snarkl-output"
          <> showDefault
      )
    <*> strOption
      ( long "constraints-output-dir"
          <> help "the directory to write the constraints.jsonl artifact"
          <> value "./snarkl-output"
          <> showDefault
      )
    <*> strOption
      ( long "inputs-output-dir"
          <> help "the directory to write the inputs.jsonl artifact"
          <> value "./snarkl-output"
          <> showDefault
      )

compile ::
  forall ty k.
  (Typeable ty) =>
  (PrimeField k) =>
  CompileOpts ->
  String ->
  Comp ty k ->
  IO ()
compile CompileOpts {..} name comp = do
  let simpl =
        catMaybes
          [ if simplify optimizeOpts then Just Simplify else Nothing,
            if removeUnreachable optimizeOpts then Just RemoveUnreachable else Nothing
          ]
      texpPkg = compileCompToTexp comp

      (r1cs, scs, privateInputMap) = compileTExpToR1CS simpl texpPkg
      publicInputs = map PublicInputVar . cs_public_in_vars . unSimplifiedConstraintSystem $ scs
      privateInputs = map (uncurry PrivateInputVar) $ Map.toList privateInputMap
      outputs = map OutputVar . cs_out_vars . unSimplifiedConstraintSystem $ scs
  let r1csFP = mkR1CSFilePath r1csOutput name
  writeFileWithDir r1csFP $ toJSONLines r1cs
  putStrLn $ "Wrote R1CS to " <> r1csFP
  let inputsFP = mkInputsFilePath inputsOutput name
  writeFileWithDir inputsFP $ toJSONLines (publicInputs <> privateInputs <> outputs)
  putStrLn $ "Wrote inputs to " <> inputsFP
