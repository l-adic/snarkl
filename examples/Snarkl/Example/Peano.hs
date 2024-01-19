{-# LANGUAGE RebindableSyntax #-}

module Snarkl.Example.Peano where

import Data.Field.Galois (GaloisField, Prime)
import GHC.TypeLits (KnownNat)
import Snarkl.Language.Prelude
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

nat_zero :: (GaloisField k) => Comp TNat k
nat_zero =
  do
    x <- inl unit
    roll x

nat_succ :: (GaloisField k) => TExp TNat k -> Comp TNat k
nat_succ n =
  do
    x <- inr n
    roll x

nat_eq ::
  (GaloisField k) =>
  Int ->
  TExp TNat k ->
  TExp TNat k ->
  Comp 'TBool k
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
                (nat_eq (dec level) n'')
                m'
          )
          n'
  | otherwise =
      return false

nat_of_int :: (GaloisField k) => Int -> Comp TNat k
nat_of_int 0 = nat_zero
nat_of_int n =
  do
    x <- nat_of_int (dec n)
    nat_succ x
