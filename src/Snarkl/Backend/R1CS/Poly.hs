{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module Snarkl.Backend.R1CS.Poly where

import qualified Data.Aeson as A
import Data.Field.Galois (GaloisField, PrimeField, fromP)
import Data.Foldable (toList)
import qualified Data.Map as Map
import Snarkl.Common
import Text.PrettyPrint.Leijen.Text (Pretty (..))

data Poly a where
  Poly :: (GaloisField a) => Assgn a -> Poly a

deriving instance (Show a) => Show (Poly a)

instance (Pretty a) => Pretty (Poly a) where
  pretty (Poly m) = pretty $ Map.toList m

instance (PrimeField a) => A.ToJSON (Poly a) where
  toJSON :: Poly a -> A.Value
  toJSON (Poly m) =
    let kvs = map (\(var, coeff) -> (show $ fromP coeff, var)) $ Map.toList m
     in A.toJSON kvs

instance (PrimeField a) => A.FromJSON (Poly a) where
  parseJSON =
    A.withArray "Poly" $ \arr -> do
      kvs <- mapM (A.parseJSON @(String, Var)) arr
      let m = Map.fromList $ map (\(k, v) -> (v, fromInteger $ read k)) (toList kvs)
      return $ Poly m

-- | The constant polynomial equal 'c'
const_poly :: (GaloisField a) => a -> Poly a
const_poly c = Poly $ Map.singleton (Var (-1)) c

-- | The polynomial equal variable 'x'
var_poly ::
  (GaloisField a) =>
  -- | Variable, with coeff
  (a, Var) ->
  -- | Resulting polynomial
  Poly a
var_poly (coeff, x) =
  Poly $ Map.singleton x coeff
