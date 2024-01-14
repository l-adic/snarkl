module Snarkl.CLI.GenWitness where

data InputOpts
  = Explicit [Integer]
  | FromFile FilePath

data GenWitnessOpts = GenWitnessOpts
  { r1csInput :: FilePath,
    inputs :: InputOpts,
    witnessOutput :: FilePath
  }