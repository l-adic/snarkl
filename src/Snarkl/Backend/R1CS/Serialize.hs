module Snarkl.Backend.R1CS.Serialize (serialize_r1cs, serialize_assgn) where

import Data.Field.Galois (PrimeField, fromP)
import qualified Data.Map as Map
import Snarkl.Backend.R1CS.Poly
import Snarkl.Backend.R1CS.R1CS
import Snarkl.Common

serialize_assgn :: (PrimeField k) => Assgn k -> String
serialize_assgn m =
  let binds = Map.toAscList $ Map.mapKeys incVar m
   in concat $
        map (\(_, v) -> show (fromP v) ++ "\n") binds

serialize_poly :: (PrimeField k) => Poly k -> String
serialize_poly p = case p of
  Poly m ->
    let size = Map.size m
        binds = Map.toList $ Map.mapKeys incVar m
        string_binds =
          map
            ( \(Var k, v) ->
                show k
                  ++ "\n"
                  ++ show (fromP v)
                  ++ "\n"
            )
            binds
     in show size
          ++ "\n"
          ++ concat string_binds

serialize_r1c :: (PrimeField k) => R1C k -> String
serialize_r1c cons = case cons of
  R1C (a, b, c) -> concat $ map serialize_poly [a, b, c]

serialize_r1cs :: (PrimeField k) => R1CS k -> String
serialize_r1cs cs =
  let r1c_strings :: String
      r1c_strings = concat (map serialize_r1c (r1cs_clauses cs))
      num_in_vars = length $ r1cs_in_vars cs
   in show num_in_vars
        ++ "\n"
        ++ show (r1cs_num_vars cs - num_in_vars)
        ++ "\n"
        ++ show (length $ r1cs_clauses cs)
        ++ "\n"
        ++ r1c_strings
