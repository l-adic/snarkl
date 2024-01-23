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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -fno-warn-unused-do-bind #-}

{-# HLINT ignore "Use if" #-}

module CompositeTest where

import qualified Prelude as P

import Data.Field.Galois (GaloisField)
import Snarkl.Language.Prelude
```

For an explaination of the intro pragmas, see the [note on rebindable syntax](../README.md#rebindablesyntax-extension-etc).


## Core Logic

The core logic here is simple, we just need a function that takes three inputs `a, b, n` and returns the boolean assertion `a * b == n`

```haskell
verifyFactors :: 
  TExp 'TField k -> 
  TExp 'TField k ->
  TExp 'TField k ->
  TExp 'TBool
verifyFactors a b n = (a * b) `eq` n
```

## Building the Program

At the top level we need to gather input then verify. 

```haskell

compositeTest :: Comp 'TBool k
compositeTest = do
   n <- fresh_public_input 
   a <- fresh_private_input "a"
   b <- fresh_public_input "b"
   return $ verifyFactors a b n
```