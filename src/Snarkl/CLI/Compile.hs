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
import Snarkl.AST (Comp)
import Snarkl.CLI.Common (mkConstraintsFilePath, mkR1CSFilePath, writeFileWithDir)
import Snarkl.Compile
  ( SimplParam (RemoveUnreachable, Simplify),
    TExpPkg (TExpPkg),
    compileCompToTexp,
    compileTExpToR1CS,
  )
import Snarkl.Constraint (ConstraintSystem (cs_constraints, cs_out_vars), SimplifiedConstraintSystem (unSimplifiedConstraintSystem))
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Toplevel (comp_interp, wit_of_cs)

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
    constraintsOutput :: FilePath
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
      TExpPkg nv in_vars e = compileCompToTexp comp
      (r1cs, cs) = compileTExpToR1CS simpl (TExpPkg nv in_vars e)
  let r1csFP = mkR1CSFilePath r1csOutput name
  writeFileWithDir r1csFP $ toJSONLines r1cs
  putStrLn $ "Wrote R1CS to " <> r1csFP
  let csFP = mkConstraintsFilePath constraintsOutput name
  writeFileWithDir csFP $ toJSONLines cs
  putStrLn $ "Wrote constraints to " <> csFP
