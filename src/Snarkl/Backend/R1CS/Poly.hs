{-# LANGUAGE InstanceSigs #-}

module Snarkl.Backend.R1CS.Poly where

import qualified Data.Aeson as A
import Data.Field.Galois (GaloisField, PrimeField, fromP)
import qualified Data.Map as Map
import Snarkl.Common
import Text.PrettyPrint.Leijen.Text (Pretty (..))

data Poly k where
  Poly :: (GaloisField k) => Assgn k -> Poly k

deriving instance Show (Poly k)

instance Pretty (Poly k) where
  pretty (Poly m) = pretty $ Map.toList m

-- The reason we use incVar is that we want to use -1 internally as the constant
-- variable (which turns out to be easier to work with internally but
-- harder to work with downstream where e.g. arkworks expects positive indices).
-- The reason we use show is because it's hard to deserialize large integers
-- in certain langauges (e.g. javascript, even rust).
instance (PrimeField k) => A.ToJSON (Poly k) where
  toJSON (Poly m) =
    let kvs = map (\(var, coeff) -> (show $ fromP coeff, incVar var)) $ Map.toList m
     in A.toJSON kvs

-- | The constant polynomial equal 'c'
const_poly :: (GaloisField k) => k -> Poly k
const_poly c = Poly $ Map.insert (Var (-1)) c Map.empty

-- | The polynomial equal variable 'x'
var_poly ::
  (GaloisField k) =>
  -- | Variable, with coeff
  (k, Var) ->
  -- | Resulting polynomial
  Poly k
var_poly (coeff, x) =
  Poly $ Map.insert x coeff Map.empty
