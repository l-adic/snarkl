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
import qualified Data.ByteString.Lazy as LBS
import Data.Field.Galois (PrimeField (fromP))
import Data.Foldable (Foldable (toList))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.String.Conversions as CS
import Data.Typeable (Typeable)
import Options.Applicative (CommandFields, Mod, Parser, command, execParser, fullDesc, header, help, helper, info, long, progDesc, strOption, subparser, switch, (<**>))
import Snarkl.Backend.R1CS
import Snarkl.Compile
import Snarkl.Constraint (ConstraintSystem (cs_out_vars), SimplifiedConstraintSystem (unSimplifiedConstraintSystem), constraintSystemToHeader, mkConstraintsFilePath, parseConstraintSystem, serializeConstraintSystemAsJson)
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Language
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
          <> help "the directory to write the r1cs artifact"
      )
    <*> strOption
      ( long "constraints-output-dir"
          <> help "the directory to write the constraints artifact"
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
      r1csFP = mkR1CSFilePath r1csOutput name
  LBS.writeFile r1csFP (serializeR1CSAsJson r1cs)
  putStrLn $ "Wrote R1CS to file " <> r1csOutput
  let csFP = mkConstraintsFilePath constraintsOutput name
  LBS.writeFile csFP (serializeConstraintSystemAsJson cs)
  putStrLn $ "Wrote constraints to file " <> constraintsOutput
