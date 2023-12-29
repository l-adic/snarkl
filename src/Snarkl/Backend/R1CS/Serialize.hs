{-# LANGUAGE RecordWildCards #-}

module Snarkl.Backend.R1CS.Serialize
  ( R1CSHeader (..),
    r1csToHeader,
    serializeInputsAsJson,
    serializeR1CSAsJson,
    serializeWitnessAsJson,
  )
where

import qualified Data.Aeson as A
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Field.Galois (GaloisField (char, deg), PrimeField, fromP)
import Data.List (sortOn)
import qualified Data.Map as Map
import JSONL (jsonLine, jsonlBuilder)
import Snarkl.Backend.R1CS.R1CS (R1CS (..), num_constraints)
import Snarkl.Common (Assgn, Var, incVar)
import Prelude hiding (writeFile)

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
      [ "field_characteristic" A..= show field_characteristic,
        "extension_degree" A..= extension_degree,
        "n_constraints" A..= n_constraints,
        "n_variables" A..= n_variables,
        "input_variables" A..= map incVar input_variables,
        "output_variables" A..= map incVar output_variables
      ]

r1csToHeader :: (GaloisField k) => R1CS k -> R1CSHeader k
r1csToHeader x@(R1CS {..} :: R1CS k) =
  R1CSHeader
    { field_characteristic = toInteger $ char (undefined :: k),
      extension_degree = toInteger $ deg (undefined :: k),
      n_constraints = num_constraints x,
      n_variables = r1cs_num_vars + length r1cs_out_vars,
      input_variables = r1cs_in_vars,
      output_variables = r1cs_out_vars
    }

serializeR1CSAsJson :: (PrimeField k) => R1CS k -> LBS.ByteString
serializeR1CSAsJson x =
  let b = jsonLine (r1csToHeader x) <> jsonlBuilder (r1cs_clauses x)
   in toLazyByteString b

serializeWitnessAsJson :: (PrimeField k) => R1CS k -> Assgn k -> LBS.ByteString
serializeWitnessAsJson r1cs assgn =
  let inputs_assgn = map (\(v, f) -> (incVar v, show $ fromP f)) $ Map.toAscList assgn
      b = jsonLine (r1csToHeader r1cs) <> jsonlBuilder inputs_assgn
   in toLazyByteString b

serializeInputsAsJson :: (PrimeField k) => R1CS k -> [k] -> LBS.ByteString
serializeInputsAsJson r1cs inputs =
  let inputs_assgn =
        map (\(v, f) -> (incVar v, show $ fromP f)) $
          sortOn fst $
            zip (r1cs_in_vars r1cs) inputs
   in toLazyByteString $ jsonlBuilder inputs_assgn
