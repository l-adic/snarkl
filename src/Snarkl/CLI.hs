{-# LANGUAGE OverloadedStrings #-}

module Snarkl.CLI (compiler) where

import Data.Field.Galois (PrimeField)
import Data.Typeable (Typeable)
import Options.Applicative (command, execParser, fullDesc, header, helper, info, progDesc, subparser, (<**>))
import Snarkl.CLI.Compile (CompileOpts, compile, compileOptsParser)
import Snarkl.CLI.GenWitness (GenWitnessOpts, genWitness, genWitnessOptsParser)
import Snarkl.CLI.RunAll (RunAllOpts, runAll, runAllOptsParser)
import Snarkl.Language

data CMD
  = Compile CompileOpts
  | GenWitness GenWitnessOpts
  | RunAll RunAllOpts

compiler ::
  forall ty k.
  (Typeable ty) =>
  (PrimeField k) =>
  String ->
  Comp ty k ->
  IO ()
compiler name comp = do
  cmd <- execParser opts
  runCMD cmd name comp
  where
    cmds =
      command "compile" (info (Compile <$> compileOptsParser <**> helper) (progDesc "Compile to program to an r1cs and constraints file"))
        <> command "gen-witness" (info (GenWitness <$> genWitnessOptsParser <**> helper) (progDesc "Generate a witness for the program"))
        <> command "run-all" (info (RunAll <$> runAllOptsParser <**> helper) (progDesc "Compile to program to an r1cs and constraints file and generate a witness"))
    opts =
      info
        (subparser cmds <**> helper)
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
  Compile opts -> compile opts name comp
  GenWitness opts -> genWitness opts name comp
  RunAll opts -> runAll opts name comp