# Generating the SNARK

In this module we show how to compile the sudoku verifier and use the CLI to generate an r1cs file and a satisfying witness.

Snarkl provides a default CLI which you can use to manage compilation and witness generation:

```haskell
{-# LANGUAGE TypeApplications #-}

import Sudoku (validatePuzzle)
import Snarkl.Field (F_BN128)
import qualified Snarkl.CLI as CLI

main :: IO ()
main = CLI.defaultMain "sudoku" (validatePuzzle @F_BN128)
```

In this case you can run

```
> cabal run exe:sudoku -- --help
```

To see the help menu. For our purposes, you will only need the `compile` and `solve` commands. 

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

and you can inspect the contents of these files. The `inputs.jsonl` file contains the data describing the required input. The public inputs correspong the the particular puzzle, the private inputs correspond to the hidden solution.


## Solve an Example Puzzle

Now we need to generate the witness, which involves solving the sudoku puzzle. In `tutorial/sudoku/solver` you can find a generic solver which is plugged in for this example (take from the stock [list of solvers](https://wiki.haskell.org/Sudoku#Simple_solver)). If you run 

```
> cabal run exe:sudoku-solver 
```

from the tutorial root directory, everything should glue with the output of the previous command and generate a file `./snarkl-output/sudoku-assignments.snarkl`. If you inspect this file, you should see the assignments for the initial puzzle state as well as a complete solution. In general solving may not involve such direct manipulation with the filesystem, but for this stand alone example it is the easiest way to procede.

## Generate a Witness

Now that we have the all of the input assignments, we can generate a witness file our r1cs:

```
> cabal run exe:sudoku -- solve --assignments-dir snarkl-output
```

which should produce the following output:

```
Wrote witness to ./snarkl-output/sudoku-witness.jsonl
```

If you inspect this file, you should recognize the assignments that were provided by the inputs (`sudoku-assignments.jsonl`). You should also verify the variable listed as `output_variables` in the files header has the assigned value `1` (indicating the programs result value that the solution is valid). At the time of writing this tutorial, the relevant portions of this file are

```json
{ "output_variables":[1486] ... }
...
[1486,"1"]

```