{-# LANGUAGE OverloadedStrings #-}

module Snarkl.CLI (defaultMain) where

import Data.Field.Galois (PrimeField)
import Data.Typeable (Typeable)
import Options.Applicative (command, execParser, fullDesc, header, helper, info, progDesc, subparser, (<**>))
import Snarkl.AST
import Snarkl.CLI.Compile (CompileOpts, compile, compileOptsParser)
import Snarkl.CLI.GenWitness (GenWitnessOpts, genWitness, genWitnessOptsParser)
import Snarkl.CLI.RunAll (RunAllOpts, runAll, runAllOptsParser)

data CMD
  = Compile CompileOpts
  | GenWitness GenWitnessOpts
  | RunAll RunAllOpts

defaultMain ::
  forall ty k.
  (Typeable ty) =>
  (PrimeField k) =>
  String ->
  Comp ty k ->
  IO ()
defaultMain name comp = do
  _opts <- execParser opts
  runCMD _opts
  where
    cmds =
      command "compile" (info (Compile <$> compileOptsParser <**> helper) (progDesc "Compile the program to an r1cs and constraint system"))
        <> command "solve" (info (GenWitness <$> genWitnessOptsParser <**> helper) (progDesc "Generate a witness for the program"))
        <> command "run-all" (info (RunAll <$> runAllOptsParser <**> helper) (progDesc "Compile the program to an r1cs and generate a witness (useful for testing)"))
    opts =
      info
        (subparser cmds <**> helper)
        ( fullDesc
            <> header ("Compiling the program " <> name <> " to a ZK-SNARK")
        )
    runCMD cmd = case cmd of
      Compile _opts -> compile _opts name comp
      GenWitness _opts -> genWitness _opts name comp
      RunAll _opts -> runAll _opts name comp
