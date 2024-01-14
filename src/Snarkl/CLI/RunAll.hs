module Snarkl.CLI.RunAll where

import Snarkl.CLI.GenWitness (InputOpts)

data RunAllOpts = RunAllOpts
  { r1csOutput :: FilePath,
    inputs :: InputOpts,
    witnessOutput :: FilePath
  }