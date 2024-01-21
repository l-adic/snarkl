module Snarkl.Backend.R1CS.R1CS
  ( R1C (..),
    R1CS (..),
    Witness (..),
    witnessInputs,
    sat_r1cs,
    num_constraints,
  )
where

import Control.Parallel.Strategies
import qualified Data.Aeson as A
import Data.Bifunctor (Bifunctor (second))
import Data.Field.Galois (GaloisField (char, deg), PrimeField)
import Data.JSONLines (FromJSONLines (fromJSONLines), ToJSONLines (toJSONLines), WithHeader (..))
import qualified Data.Map as Map
import Snarkl.Backend.R1CS.Poly
import Snarkl.Common
import Text.PrettyPrint.Leijen.Text (Pretty (..), vsep, (<+>))

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
    r1cs_public_in_vars :: [Var],
    r1cs_out_vars :: [Var]
  }
  deriving (Show)

instance (PrimeField k) => ToJSONLines (R1CS k) where
  toJSONLines cs = toJSONLines $ WithHeader (r1csHeader cs) (r1cs_clauses cs)
    where
      r1csHeader (_ :: R1CS a) =
        ConstraintHeader
          { field_characteristic = toInteger $ char (undefined :: a),
            extension_degree = toInteger $ deg (undefined :: a),
            n_constraints = num_constraints cs,
            n_variables = r1cs_num_vars cs,
            input_variables = r1cs_public_in_vars cs,
            output_variables = r1cs_out_vars cs
          }

instance (Pretty a) => Pretty (R1CS a) where
  pretty (R1CS {..}) = vsep (pretty <$> r1cs_clauses)

instance (PrimeField k) => FromJSONLines (R1CS k) where
  fromJSONLines ls = do
    WithHeader ConstraintHeader {..} cs <- fromJSONLines ls
    pure
      R1CS
        { r1cs_clauses = cs,
          r1cs_num_vars = fromIntegral n_variables,
          r1cs_public_in_vars = input_variables,
          r1cs_out_vars = output_variables
        }

data Witness k = Witness
  { witness_assgn :: Assgn k,
    witness_in_vars :: [Var],
    witness_out_vars :: [Var],
    witness_num_vars :: Int
  }
  deriving (Show)

instance (Pretty k) => Pretty (Witness k) where
  pretty (Witness {..}) =
    pretty witness_assgn

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
      [ "input_variables" A..= in_vars,
        "output_variables" A..= out_vars,
        "n_variables" A..= num_vars
      ]

instance A.FromJSON WitnessHeader where
  parseJSON =
    A.withObject "WitnessHeader" $ \v -> do
      in_vars <- v A..: "input_variables"
      out_vars <- v A..: "output_variables"
      num_vars <- v A..: "n_variables"
      pure $ WitnessHeader in_vars out_vars num_vars

instance (PrimeField k) => ToJSONLines (Witness k) where
  toJSONLines wit@(Witness {witness_assgn = Assgn m}) =
    toJSONLines $
      WithHeader
        (witnessHeader wit)
        (Map.toList (FieldElem <$> m))
    where
      witnessHeader (Witness {..}) =
        WitnessHeader
          { in_vars = witness_in_vars,
            out_vars = witness_out_vars,
            num_vars = witness_num_vars
          }

instance (PrimeField k) => FromJSONLines (Witness k) where
  fromJSONLines ls = do
    WithHeader WitnessHeader {..} assgn <- fromJSONLines ls
    pure
      Witness
        { witness_assgn = Assgn $ Map.fromList (second unFieldElem <$> assgn),
          witness_in_vars = in_vars,
          witness_out_vars = out_vars,
          witness_num_vars = num_vars
        }

witnessInputs :: Witness k -> Assgn k
witnessInputs (Witness {witness_in_vars, witness_assgn = Assgn m}) =
  Assgn $ Map.filterWithKey (\k _ -> k `elem` witness_in_vars) m

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
    is_sat cs0 = map (sat_r1c w) cs0 `using` parListChunk (chunk_sz cs0) rseq
    num_chunks = 32
    chunk_sz cs0 =
      truncate $ (fromIntegral (length cs0) :: Rational) / num_chunks
