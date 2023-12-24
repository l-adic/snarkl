{-# LANGUAGE InstanceSigs #-}

module Snarkl.Backend.R1CS.Poly where

import qualified Data.Aeson as A
import Data.Field.Galois (GaloisField, PrimeField, fromP)
import qualified Data.Map as Map
import Snarkl.Common

data Poly a where
  Poly :: (GaloisField a) => Assgn a -> Poly a

instance (Show a) => Show (Poly a) where
  show (Poly m) = show m

instance (PrimeField a) => A.ToJSON (Poly a) where
  toJSON :: Poly a -> A.Value
  toJSON (Poly m) =
    let kvs = map (\(var, coeff) -> (fromP coeff, show var)) $ Map.toList m
     in A.toJSON kvs

-- | The constant polynomial equal 'c'
const_poly :: (GaloisField a) => a -> Poly a
const_poly c = Poly $ Map.insert (Var (-1)) c Map.empty

-- | The polynomial equal variable 'x'
var_poly ::
  (GaloisField a) =>
  -- | Variable, with coeff
  (a, Var) ->
  -- | Resulting polynomial
  Poly a
var_poly (coeff, x) =
  Poly $ Map.insert x coeff Map.empty
