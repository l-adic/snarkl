{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snarkl.Backend.R1CS.R1CS
  ( R1C (..),
    R1CS (..),
    sat_r1cs,
    num_constraints,
  )
where

import Control.Parallel.Strategies
import qualified Data.Aeson as A
import Data.Field.Galois (GaloisField, PrimeField)
import qualified Data.IntMap.Lazy as Map
import Snarkl.Backend.R1CS.Poly
import Snarkl.Common
import Snarkl.Errors

----------------------------------------------------------------
--                Rank-1 Constraint Systems                   --
----------------------------------------------------------------

data R1C a where
  R1C :: (GaloisField a) => (Poly a, Poly a, Poly a) -> R1C a

instance (PrimeField a) => A.ToJSON (R1C a) where
  toJSON (R1C (aV, bV, cV)) =
    A.object
      [ "A" A..= aV,
        "B" A..= bV,
        "C" A..= cV
      ]

instance (Show a) => Show (R1C a) where
  show (R1C (aV, bV, cV)) = show aV ++ "*" ++ show bV ++ "==" ++ show cV

{-

{
 "r1cs":{
 "version":"1.0",
 "field_characteristic":"133581199851807797997178235848527563401",
 "extension_degree":1,
 "instances":3,
 "witnesses":5,
 "constraints":2000,
 "optimized":true
0 }

-}

data R1CS a = R1CS
  { r1cs_clauses :: [R1C a],
    r1cs_num_vars :: Int,
    r1cs_in_vars :: [Var],
    r1cs_out_vars :: [Var],
    r1cs_gen_witness :: Assgn a -> Assgn a
  }

instance (Show a) => Show (R1CS a) where
  show (R1CS cs nvs ivs ovs _) = show (cs, nvs, ivs, ovs)

num_constraints :: R1CS a -> Int
num_constraints = length . r1cs_clauses

-- sat_r1c: Does witness 'w' satisfy constraint 'c'?
sat_r1c :: (GaloisField a) => Assgn a -> R1C a -> Bool
sat_r1c w c
  | R1C (aV, bV, cV) <- c =
      inner aV w * inner bV w == inner cV w
  where
    inner :: (GaloisField a) => Poly a -> Assgn a -> a
    inner (Poly v) w' =
      let c0 = Map.findWithDefault 0 (-1) v
       in Map.foldlWithKey (f w') c0 v

    f w' acc v_key v_val =
      (v_val * Map.findWithDefault 0 v_key w') + acc

-- sat_r1cs: Does witness 'w' satisfy constraint set 'cs'?
sat_r1cs :: (GaloisField a) => Assgn a -> R1CS a -> Bool
sat_r1cs w cs = all id $ is_sat (r1cs_clauses cs)
  where
    is_sat cs0 = map g cs0 `using` parListChunk (chunk_sz cs0) rseq
    num_chunks = 32
    chunk_sz cs0 =
      truncate $ (fromIntegral (length cs0) :: Rational) / num_chunks
    g c =
      if sat_r1c w c
        then True
        else
          failWith $
            ErrMsg
              ( "witness\n  "
                  ++ show w
                  ++ "\nfailed to satisfy constraint\n  "
                  ++ show c
                  ++ "\nin R1CS\n  "
                  ++ show cs
              )
