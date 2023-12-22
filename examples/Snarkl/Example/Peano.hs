{-# LANGUAGE RebindableSyntax #-}

module Snarkl.Example.Peano where

import Data.Field.Galois (Prime)
import GHC.TypeLits (KnownNat)
import Snarkl.Language.Syntax
import Snarkl.Language.SyntaxMonad
import Snarkl.Language.TExpr
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

type TF = 'TFSum ('TFConst 'TUnit) 'TFId

type TNat = 'TMu TF

nat_zero :: (KnownNat p) => Comp TNat p
nat_zero =
  do
    x <- inl unit
    roll x

nat_succ :: (KnownNat p) => TExp TNat (Prime p) -> Comp TNat p
nat_succ n =
  do
    x <- inr n
    roll x

nat_eq ::
  (KnownNat p) =>
  Int ->
  TExp TNat (Prime p) ->
  TExp TNat (Prime p) ->
  Comp 'TBool p
nat_eq level n m
  | level > 0 =
      do
        n' <- unroll n
        m' <- unroll m
        case_sum
          (const $ case_sum (const $ return true) (const $ return false) m')
          ( \n'' ->
              case_sum
                (const $ return false)
                (\m'' -> nat_eq (dec level) n'' m'')
                m'
          )
          n'
  | otherwise =
      return false

nat_of_int :: (KnownNat p) => Int -> Comp TNat p
nat_of_int 0 = nat_zero
nat_of_int n =
  do
    x <- nat_of_int (dec n)
    nat_succ x
