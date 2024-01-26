# Generating the SNARK

In this module we show how to compile the sudoku verifier and use the CLI to generate an r1cs file and a satisfying witness.

Snarkl provides a default CLI which you can use to manage compilation and witness generation:

```haskell
{-# LANGUAGE TypeApplications #-}

import Sudoku (validatePuzzle)
import Snarkl.Field (F_BN128)
import qualified Snarkl.CLI as CLI

main :: IO ()
main = CLI.defaultMain "sudoku" $ validatePuzzle @F_BN128
```

In this case you can run

```
> cabal run exe:sudoku -- --help
```

to see the help menu. You should see something like the following output:

```
Compiling the program sudoku to a ZK-SNARK

Usage: sudoku COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  compile                  Compile the program to an r1cs and constraint system
  solve                    Generate a witness for the program
  run-all                  Compile the program to an r1cs and generate a witness
                           (useful for testing)
```


For our purposes, you will only need the `compile` and `solve` commands. 

## Generating the R1CS

To generate the r1cs file, you can run

```
> cabal run exe:sudoku -- compile
```

You should see the following console output

```
Wrote R1CS to ./snarkl-output/sudoku-r1cs.jsonl
Wrote inputs to ./snarkl-output/sudoku-inputs.jsonl
```

and you can inspect the contents of these files. The `inputs.jsonl` file contains the data describing the required input. The public inputs correspond the the particular puzzle, the private inputs correspond to the hidden solution.


## Solve an Example Puzzle

Now we need to generate the witness, which involves solving a sudoku puzzle. We provide a [generic solver](./solver/Main.hs) which is customized to work with this example. The core logic is taken from the stock [list of solvers](https://wiki.haskell.org/Sudoku#Simple_solver). If you run

```
> cabal run exe:sudoku-solver 
```

from the tutorial root directory, everything should glue with the output of the previous command to generate a file `./snarkl-output/sudoku-assignments.snarkl`. If you inspect this file, you should see the assignments for the initial puzzle state as well as a complete solution to the r1cs. In general solving may not involve directly using the filesystem like this, but for this stand alone example it is the easiest way to proceed.

## Generate a Witness

Now that we have the all of the input assignments, we can generate a witness file for our r1cs:

```
> cabal run exe:sudoku -- solve --assignments-dir snarkl-output
```

which should produce the following output:

```
Wrote witness to ./snarkl-output/sudoku-witness.jsonl
```

If you inspect this file, you should recognize the assignments that were provided by (`sudoku-assignments.jsonl`). You should also check the variable listed as `output_variables` in the file header has the assigned value `1` (indicating the program's result value, where the field element `1` indicates `true`). At the time of writing this tutorial, the relevant portions of this file are

```json
{ "output_variables":[1486] ... }
...
[1486,"1"]

```

## Proof Validation on Ethereum
If you would like to use these artifacts for Ethereum compatible proof and verification, checkout out the sample at [purescript-zk](https://github.com/l-adic/purescript-zk). That example is using artifacts from this tutorial.
