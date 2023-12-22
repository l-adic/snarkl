{-# LANGUAGE RebindableSyntax #-}

module Snarkl.Example.Basic where

import Data.Field.Galois (Prime)
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownNat)
import Snarkl.Compile
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
  TExp 'TField (Prime p) ->
  TExp 'TField (Prime p) ->
  Comp 'TField p
mult_ex x y = return $ x * y

arr_ex :: (KnownNat p) => TExp 'TField (Prime p) -> Comp 'TField p
arr_ex x = do
  a <- arr 2
  forall [0 .. 1] (\i -> set (a, i) x)
  y <- get (a, 0)
  z <- get (a, 1)
  return $ y + z

p1 :: (KnownNat p) => Comp 'TField p
p1 = arr_ex $ fromPrimeField 1

desugar1 :: (KnownNat p) => TExpPkg 'TField p
desugar1 = texp_of_comp p1

interp1 :: (KnownNat p) => Prime p
interp1 = comp_interp p1 []

p2 = do
  x <- fresh_input
  return $ x + x

desugar2 = texp_of_comp p2

interp2 :: (KnownNat p) => Prime p
interp2 = comp_interp p2 []

interp2' :: (KnownNat p) => Prime p
interp2' = comp_interp p2 [256]

compile1 :: (KnownNat p) => R1CS (Prime p)
compile1 = r1cs_of_comp Simplify p1

run1 :: IO ExitCode
run1 = snarkify_comp "example" Simplify p1 ([] :: [Prime 257])

comp1 :: (KnownNat p, Typeable a) => Comp ('TSum 'TBool a) p
comp1 = inl false

comp2 :: (KnownNat p, Typeable a) => Comp ('TSum a 'TField) p
comp2 = inr (fromPrimeField 0)

test1 :: (KnownNat p) => State (Env p) (TExp 'TBool (Prime p))
test1 = do
  b <- fresh_input
  z <- if return b then comp1 else comp2
  case_sum (\x0 -> return x0) (\_ -> return false) z
