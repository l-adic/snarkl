module Snarkl.Backend.R1CS.R1CS
  ( R1C (..),
    R1CS (..),
    sat_r1cs,
    wit_of_r1cs,
    num_constraints,
  )
where

import Control.Parallel.Strategies
import qualified Data.Aeson as A
import Data.Field.Galois (GaloisField, PrimeField)
import qualified Data.Map as Map
import Snarkl.Backend.R1CS.Poly
import Snarkl.Common
import Snarkl.Errors
import Text.PrettyPrint.Leijen.Text (Pretty (..), (<+>))

----------------------------------------------------------------
--                Rank-1 Constraint Systems                   --
----------------------------------------------------------------

data R1C k where
  R1C :: (GaloisField k) => (Poly k, Poly k, Poly k) -> R1C k

deriving instance (Show k) => Show (R1C k)

instance (PrimeField k) => A.ToJSON (R1C k) where
  toJSON (R1C (a, b, c)) =
    A.object
      [ "A" A..= a,
        "B" A..= b,
        "C" A..= c
      ]

instance Pretty (R1C k) where
  pretty (R1C (aV, bV, cV)) = pretty aV <+> "*" <+> pretty bV <+> "==" <+> pretty cV

data R1CS a = R1CS
  { r1cs_clauses :: [R1C a],
    r1cs_num_vars :: Int,
    r1cs_in_vars :: [Var],
    r1cs_out_vars :: [Var],
    r1cs_known_vars :: Map.Map String Var,
    r1cs_gen_witness :: Assgn a -> Assgn a
  }

instance (Show k) => Show (R1CS k) where
  show (R1CS cs nvs ivs ovs known _) = show (cs, nvs, ivs, ovs, known)

num_constraints :: R1CS k -> Int
num_constraints = length . r1cs_clauses

-- sat_r1c: Does witness 'w' satisfy constraint 'c'?
sat_r1c :: (GaloisField k) => Assgn k -> R1C k -> Bool
sat_r1c w c
  | R1C (aV, bV, cV) <- c =
      inner aV w * inner bV w == inner cV w
  where
    inner :: (GaloisField k) => Poly k -> Assgn k -> k
    inner (Poly v) w' =
      let c0 = Map.findWithDefault 0 (Var (-1)) v
       in Map.foldlWithKey (f w') c0 v

    f w' acc v_key v_val =
      (v_val * Map.findWithDefault 0 v_key w') + acc

-- sat_r1cs: Does witness 'w' satisfy constraint set 'cs'?
sat_r1cs :: (GaloisField k) => Assgn k -> R1CS k -> Bool
sat_r1cs w cs = and $ is_sat (r1cs_clauses cs)
  where
    is_sat cs0 = map g cs0 `using` parListChunk (chunk_sz cs0) rseq
    num_chunks = 32
    chunk_sz cs0 =
      truncate $ (fromIntegral (length cs0) :: Rational) / num_chunks
    g c =
      sat_r1c w c
        || failWith
          ( ErrMsg
              ( "witness\n  "
                  ++ show w
                  ++ "\nfailed to satisfy constraint\n  "
                  ++ show c
                  ++ "\nin R1CS\n  "
                  ++ show cs
              )
          )

-- | For a given R1CS and inputs, calculate a satisfying assignment.
wit_of_r1cs :: [k] -> Map.Map String k -> R1CS k -> Assgn k
wit_of_r1cs inputs knownAssignments r1cs =
  let in_vars = r1cs_in_vars r1cs
      f = r1cs_gen_witness r1cs . Map.fromList
      initAssignments =
        [ (var, val)
          | (k, val) <- Map.toList knownAssignments,
            (k', var) <- Map.toList (r1cs_known_vars r1cs),
            k == k'
        ]
   in if length in_vars /= length inputs
        then
          failWith $
            ErrMsg
              ( "expected "
                  ++ show (length in_vars)
                  ++ " input(s)"
                  ++ " but got "
                  ++ show (length inputs)
                  ++ " input(s)"
              )
        else f (initAssignments <> zip in_vars inputs)
