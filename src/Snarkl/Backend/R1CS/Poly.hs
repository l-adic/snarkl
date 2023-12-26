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

-- The reason we use incVar is that we want to use -1 as the constant
-- coefficient (which turns out to be easier to work with internally but
-- harder to work with downstream).
-- The reason we use show is because it's hard to deserialize large integers
-- in certain langauges (e.g. javascript, even rust).
instance (PrimeField a) => A.ToJSON (Poly a) where
  toJSON :: Poly a -> A.Value
  toJSON (Poly m) =
    let kvs = map (\(var, coeff) -> (show $ fromP coeff, incVar var)) $ Map.toList m
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
