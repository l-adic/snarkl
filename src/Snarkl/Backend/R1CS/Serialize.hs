{-# LANGUAGE RecordWildCards #-}

module Snarkl.Backend.R1CS.Serialize (serialize_r1cs, serialize_assgn, serializeR1CSAsJson) where

import qualified Data.Aeson as A
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (writeFile)
import Data.Field.Galois (GaloisField (char, deg), PrimeField, fromP)
import qualified Data.Map as Map
import JSONL (jsonLine, jsonlBuilder)
import Snarkl.Backend.R1CS.Poly
import Snarkl.Backend.R1CS.R1CS
import Snarkl.Common
import Prelude hiding (writeFile)

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

--------------------------------------------------------------------------------

data R1CSHeader k = R1CSHeader
  { field_characteristic :: Integer,
    extension_degree :: Integer,
    n_constraints :: Int,
    n_variables :: Int,
    input_variables :: [Var],
    output_variables :: [Var]
  }

instance (GaloisField k) => A.ToJSON (R1CSHeader k) where
  toJSON (R1CSHeader {..}) =
    A.object
      [ "field_characteristic" A..= field_characteristic,
        "extension_degree" A..= extension_degree,
        "n_constraints" A..= n_constraints,
        "n_variables" A..= n_variables,
        "input_variables" A..= input_variables,
        "output_variables" A..= output_variables
      ]

r1csToHeader :: (GaloisField k) => R1CS k -> R1CSHeader k
r1csToHeader x@(R1CS {..} :: R1CS k) =
  R1CSHeader
    { field_characteristic = toInteger $ char (undefined :: k),
      extension_degree = toInteger $ deg (undefined :: k),
      n_constraints = num_constraints x,
      n_variables = r1cs_num_vars,
      input_variables = r1cs_in_vars,
      output_variables = r1cs_out_vars
    }

serializeR1CSAsJson :: (PrimeField k) => FilePath -> R1CS k -> IO ()
serializeR1CSAsJson fp x =
  let b = jsonLine (r1csToHeader x) <> jsonlBuilder (r1cs_clauses x)
   in writeFile fp (toLazyByteString b)
