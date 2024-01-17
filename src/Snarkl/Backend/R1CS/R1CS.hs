module Snarkl.Backend.R1CS.R1CS
  ( R1C (..),
    R1CS (..),
    r1csHeader,
    Witness (..),
    witnessHeader,
    WitnessHeader (..),
    sat_r1cs,
    num_constraints,
  )
where

import Control.Parallel.Strategies
import qualified Data.Aeson as A
import Data.Field.Galois (GaloisField (char, deg), PrimeField)
import qualified Data.Map as Map
import Snarkl.Backend.R1CS.Poly
import Snarkl.Common
import Text.PrettyPrint.Leijen.Text (Pretty (..), (<+>))

----------------------------------------------------------------
--                Rank-1 Constraint Systems                   --
----------------------------------------------------------------

data R1C a where
  R1C :: (GaloisField a) => (Poly a, Poly a, Poly a) -> R1C a

deriving instance (Show a) => Show (R1C a)

instance (PrimeField k) => A.ToJSON (R1C k) where
  toJSON (R1C (a, b, c)) =
    A.object
      [ "A" A..= a,
        "B" A..= b,
        "C" A..= c
      ]

instance (PrimeField k) => A.FromJSON (R1C k) where
  parseJSON =
    A.withObject "R1C" $ \v -> do
      a <- v A..: "A"
      b <- v A..: "B"
      c <- v A..: "C"
      pure $ R1C (a, b, c)

instance (Pretty a) => Pretty (R1C a) where
  pretty (R1C (aV, bV, cV)) = pretty aV <+> "*" <+> pretty bV <+> "==" <+> pretty cV

data R1CS a = R1CS
  { r1cs_clauses :: [R1C a],
    r1cs_num_vars :: Int,
    r1cs_in_vars :: [Var],
    r1cs_out_vars :: [Var]
  }
  deriving (Show)

r1csHeader :: (GaloisField a) => R1CS a -> ConstraintHeader
r1csHeader (cs :: R1CS a) =
  ConstraintHeader
    { field_characteristic = toInteger $ char (undefined :: a),
      extension_degree = toInteger $ deg (undefined :: a),
      n_constraints = length (r1cs_clauses cs),
      n_variables = r1cs_num_vars cs,
      input_variables = r1cs_in_vars cs,
      output_variables = r1cs_out_vars cs
    }

data Witness k = Witness
  { witness_assgn :: Assgn k,
    witness_in_vars :: [Var],
    witness_out_vars :: [Var],
    witness_num_vars :: Int
  }
  deriving (Show)

instance Functor Witness where
  fmap f w = w {witness_assgn = fmap f (witness_assgn w)}

data WitnessHeader = WitnessHeader
  { in_vars :: [Var],
    out_vars :: [Var],
    num_vars :: Int
  }

instance A.ToJSON WitnessHeader where
  toJSON (WitnessHeader in_vars out_vars num_vars) =
    A.object
      [ "in_vars" A..= in_vars,
        "out_vars" A..= out_vars,
        "num_vars" A..= num_vars
      ]

instance A.FromJSON WitnessHeader where
  parseJSON =
    A.withObject "WitnessHeader" $ \v -> do
      in_vars <- v A..: "in_vars"
      out_vars <- v A..: "out_vars"
      num_vars <- v A..: "num_vars"
      pure $ WitnessHeader in_vars out_vars num_vars

witnessHeader :: Witness k -> WitnessHeader
witnessHeader (Witness {..}) =
  WitnessHeader
    { in_vars = witness_in_vars,
      out_vars = witness_out_vars,
      num_vars = witness_num_vars
    }

num_constraints :: R1CS a -> Int
num_constraints = length . r1cs_clauses

-- sat_r1c: Does witness 'w' satisfy constraint 'c'?
sat_r1c :: (GaloisField a) => Witness a -> R1C a -> Bool
sat_r1c w c
  | R1C (aV, bV, cV) <- c =
      inner aV w * inner bV w == inner cV w
  where
    inner :: (GaloisField a) => Poly a -> Witness a -> a
    inner (Poly (Assgn v)) (Witness {witness_assgn = Assgn wit}) =
      let c0 = Map.findWithDefault 0 (Var (-1)) v
       in Map.foldlWithKey (f wit) c0 v

    f wit acc v_key v_val =
      (v_val * Map.findWithDefault 0 v_key wit) + acc

-- sat_r1cs: Does witness 'w' satisfy constraint set 'cs'?
sat_r1cs :: (GaloisField a) => Witness a -> R1CS a -> Bool
sat_r1cs w cs = and $ is_sat (r1cs_clauses cs)
  where
    is_sat cs0 = map g cs0 `using` parListChunk (chunk_sz cs0) rseq
    num_chunks = 32
    chunk_sz cs0 =
      truncate $ (fromIntegral (length cs0) :: Rational) / num_chunks
    g c =
      sat_r1c w c
