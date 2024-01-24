# Composite Test

We are going to write program which verifies that a number is composite, i.e. is the product of two numbers which are greater than `1`. This plays the role of `HelloWorld` in the ZK setting.

More formally, the program will accept

-  1 public input `n`, which is the number to be factored
-  2 private inputs `a` and `b` which have the property that `a * b == n`

## Setup

The following import statements and language extensions are required:


```haskell
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -fno-warn-unused-do-bind #-}

{-# HLINT ignore "Use if" #-}

module Main where

import Prelude (IO, print, fromInteger, ($))

import Data.Field.Galois (PrimeField)
import qualified Data.Map as Map
import Snarkl.Field (F_BN128)
import Snarkl.Language.Prelude
import Snarkl.Toplevel (Result(..), execute)
import Text.PrettyPrint.Leijen.Text (Pretty (..))
```

For an explaination of the intro pragmas, see the [note on rebindable syntax](../README.md#rebindablesyntax-extension-etc).


## Core Logic

The core logic here is simple, we just need a function that takes three inputs `a, b, n` and returns the boolean assertion `a * b == n`

```haskell
verifyFactors :: 
  TExp 'TField k -> 
  TExp 'TField k ->
  TExp 'TField k ->
  TExp 'TBool k
verifyFactors a b n = (a * b) `eq` n
```

For an explaination on why `'TField` and `'TBool` (as well as other primitive types) use "tick marks", see [this note](../README.md/#types-with-tick-marks)

## Building the Program

At the top level we need to gather input then verify. 

```haskell
compositeTest :: Comp 'TBool k
compositeTest = do
   n <- fresh_public_input 
   a <- fresh_private_input "a"
   b <- fresh_private_input "b"
   return $ verifyFactors a b n
```

## Run The program

We can run the program by supplying the public and private input:

```haskell
runCompositeTest :: 
  PrimeField k =>
  k ->
  -- ^ public input n
  (k,k) ->
  -- ^ private input (a,b)
  Result k
runCompositeTest n (a,b) = 
  execute [] compositeTest [n] (Map.fromList [("a", a), ("b", b)])

main :: IO ()
main = print $ pretty $ runCompositeTest @F_BN128 10 (2,5)
```

- The first argument to `execute` is the list of optimizations that we want to run on 
  the program, which in this case is none.
- Notice that the private input arguments must be named with the same names which introduced
  the variables.

For an explaination on the `PrimeField` constraint, see the note on [Galois Fields](../README.md#galois-fields).

The definition of `Result k` is given as

```haskell ignore

data Result k = Result
  { result_sat :: Bool,
    result_vars :: Int,
    result_constraints :: Int,
    result_result :: k,
    result_r1cs :: R1CS k,
    result_witness :: Witness k
  }
```

where some important fields are:

- `result_r1cs`: The r1cs generated by the program
- `result_witness`: A variable assignment to all variables which satisfies the r1cs
- `result_result`: The value that the computation results in.

For a concrete example, try running 

```
> cabal run exe:composite-test
```

You should see something like the following output:

```
is satisfied = True
number of vars = 9
number of constraints = 6
result = 1
r1cs :
    1 * (-1 * x_1 + x_5 + -1 * x_6) == 0
    1 * (1 + -1 * x_4 + -1 * x_7) == 0
    1 * (1 + -1 * x_7 + -1 * x_9) == 0
    x_2 * x_3 == x_5
    x_6 * x_8 == x_7
    x_9 * x_6 == 0
witness :
    x_1 := 10
    x_2 := 2
    x_3 := 5
    x_4 := 1
    x_5 := 10
    x_6 := 0
    x_7 := 0
    x_8 := 0
    x_9 := 1

```

**NOTE: Unfortunately you might see something like `21888242871839275222246405745257275088548364400416034343698204186575808495616` instead of `-1`. This is ok as these are equal in `F_BN128`, but it's annoying from a pretty printing point of view.