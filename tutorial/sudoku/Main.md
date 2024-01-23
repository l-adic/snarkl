# Generating the SNARK

In this module we show how to compile the sudoku verifier and use the CLI to generate an r1cs file and a satisfying witness.

We will need an example 

```haskell

{-# LANGUAGE TypeApplications, FlexibleContexts #-}

import Sudoku (validatePuzzle)
import Snarkl.Field (F_BN128)
import qualified Snarkl.CLI as CLI

main :: IO ()
main = CLI.defaultMain "sudoku" (validatePuzzle @F_BN128)