{-# LANGUAGE RebindableSyntax #-}

module Snarkl.Example.Basic where

import Data.Field.Galois (GaloisField, Prime)
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownNat)
import Snarkl.Arkworks (CMD (RunR1CS), runCMD)
import Snarkl.Compile
import Snarkl.Field (F_BN128)
import Snarkl.Language.Syntax
import Snarkl.Language.SyntaxMonad
import Snarkl.Language.TExpr
import Snarkl.Toplevel
import System.Exit (ExitCode)
import Prelude hiding
  ( fromRational,
    negate,
    return,
    (&&),
    (*),
    (+),
    (-),
    (/),
    (>>),
    (>>=),
  )

mult_ex ::
  TExp 'TField k ->
  TExp 'TField k ->
  Comp 'TField k
mult_ex x y = return $ x * y

arr_ex :: (GaloisField k) => TExp 'TField k -> Comp 'TField k
arr_ex x = do
  a <- arr 2
  forall [0 .. 1] (\i -> set (a, i) x)
  y <- get (a, 0)
  z <- get (a, 1)
  return $ y + z

p1 :: (GaloisField k) => Comp 'TField k
p1 = arr_ex $ fromField 1

desugar1 :: (GaloisField k) => TExpPkg 'TField k
desugar1 = compileCompToTexp p1

interp1 :: (GaloisField k) => k
interp1 = comp_interp p1 []

p2 = do
  x <- fresh_input
  return $ x + x

desugar2 = compileCompToTexp p2

interp2 :: (GaloisField k) => k
interp2 = comp_interp p2 []

interp2' :: (GaloisField k) => k
interp2' = comp_interp p2 [256]

compile1 :: (GaloisField k) => R1CS k
compile1 = compileCompToR1CS Simplify p1

run1 :: IO ExitCode
run1 = runCMD $ RunR1CS "./scripts" "example" Simplify p1 ([] :: [F_BN128])

comp1 :: (GaloisField k, Typeable a) => Comp ('TSum 'TBool a) k
comp1 = inl false

comp2 :: (GaloisField k, Typeable a) => Comp ('TSum a 'TField) k
comp2 = inr (fromField 0)

test1 :: (GaloisField k) => State (Env k) (TExp 'TBool k)
test1 = do
  b <- fresh_input
  z <- if return b then comp1 else comp2
  case_sum return (const $ return false) z
