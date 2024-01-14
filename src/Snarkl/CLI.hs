{-# LANGUAGE RecordWildCards #-}

module Snarkl.CLI (compiler) where

import qualified Data.ByteString.Lazy as LBS
import Data.Field.Galois (PrimeField)
import Data.Maybe (catMaybes)
import Data.Typeable (Typeable)
import Options.Applicative (execParser, fullDesc, header, helper, info, (<**>))
import Snarkl.Backend.R1CS
import Snarkl.CLI.Compile (CompileOpts (..), OptimiseOpts (..), compileOptsParser)
import Snarkl.CLI.GenWitness (GenWitnessOpts (..))
import Snarkl.Compile
import Snarkl.Constraint ()
import Snarkl.Language

data CMD
  = Compile CompileOpts
  | GenWitness GenWitnessOpts

compiler ::
  forall ty k.
  (Typeable ty) =>
  (PrimeField k) =>
  String ->
  Comp ty k ->
  IO ()
compiler name comp = do
  compileOpts <- execParser opts
  runCMD (Compile compileOpts) name comp
  where
    opts =
      info
        (compileOptsParser <**> helper)
        ( fullDesc
            <> header ("Compiling the program " <> name <> " to a ZK-SNARK")
        )

runCMD ::
  forall ty k.
  (Typeable ty) =>
  (PrimeField k) =>
  CMD ->
  String ->
  Comp ty k ->
  IO ()
runCMD cmd name comp = case cmd of
  Compile (CompileOpts {..}) -> do
    let simpl =
          catMaybes
            [ if simplify optimiseOpts then Just Simplify else Nothing,
              if removeUnreachable optimiseOpts then Just RemoveUnreachable else Nothing
            ]
        TExpPkg nv in_vars e = compileCompToTexp comp
        r1cs = compileTExpToR1CS simpl (TExpPkg nv in_vars e)
        r1csFP = mkR1CSFilePath name r1csOutput
    LBS.writeFile r1csFP (serializeR1CSAsJson r1cs)
    putStrLn $ "Wrote R1CS to file " <> r1csOutput
  GenWitness (GenWitnessOpts {..}) -> do
    