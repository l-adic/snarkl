module Snarkl.Backend.R1CS.Poly where

import qualified Data.Aeson as A
import Data.Field.Galois (GaloisField, PrimeField)
import qualified Data.Map as Map
import Snarkl.Common
import Text.PrettyPrint.Leijen.Text (Pretty (..), hsep, parens, punctuate, (<+>))

data Poly a where
  Poly :: (GaloisField a) => Assgn a -> Poly a

deriving instance (Show a) => Show (Poly a)

instance (PrimeField a) => A.ToJSON (Poly a) where
  toJSON (Poly m) = A.toJSON m

instance (PrimeField a) => A.FromJSON (Poly a) where
  parseJSON v = Poly <$> A.parseJSON v

instance (Pretty a) => Pretty (Poly a) where
  pretty (Poly (Assgn m)) =
    let mkVar v = if v == Var (-1) then "1" else pretty v
        summands =
          map
            ( \(var, coeff) ->
                if coeff == 1
                  then mkVar var
                  else
                    if var == Var (-1)
                      then pretty coeff
                      else pretty coeff <+> "*" <+> mkVar var
            )
            $ filter (\(_, coeff) -> coeff /= 0)
            $ Map.toList m
        summands' = if null summands then ["0"] else summands
     in case summands' of
          [] -> "0"
          [x] -> x
          xs -> parens (hsep $ punctuate " +" xs)

-- | The constant polynomial equal 'c'
const_poly :: (GaloisField a) => a -> Poly a
const_poly c = Poly $ Assgn $ Map.singleton (Var (-1)) c

-- | The polynomial equal variable 'x'
var_poly ::
  (GaloisField a) =>
  -- | Variable, with coeff
  (a, Var) ->
  -- | Resulting polynomial
  Poly a
var_poly (coeff, x) =
  Poly $ Assgn $ Map.singleton x coeff
