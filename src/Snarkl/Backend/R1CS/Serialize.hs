{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Snarkl.Backend.R1CS.Serialize where

import qualified Data.Aeson as A
import Data.Field.Galois (GaloisField (char, deg))
import GHC.Generics (Generic)
import Snarkl.Backend.R1CS.R1CS (R1CS (..), num_constraints)
import Snarkl.Common (Var)

data ConstraintHeader k = ConstraintHeader
  { field_characteristic :: Integer,
    extension_degree :: Integer,
    n_constraints :: Int,
    n_variables :: Int,
    input_variables :: [Var],
    output_variables :: [Var]
  }
  deriving (Generic)

instance A.ToJSON (ConstraintHeader k)

instance A.FromJSON (ConstraintHeader k)

r1csToHeader :: (GaloisField k) => R1CS k -> ConstraintHeader k
r1csToHeader x@(R1CS {..} :: R1CS k) =
  ConstraintHeader
    { field_characteristic = toInteger $ char (undefined :: k),
      extension_degree = toInteger $ deg (undefined :: k),
      n_constraints = num_constraints x,
      n_variables = r1cs_num_vars + length r1cs_out_vars,
      input_variables = r1cs_in_vars,
      output_variables = r1cs_out_vars
    }

mkR1CSFilePath :: FilePath -> String -> FilePath
mkR1CSFilePath rootDir name = rootDir <> "/" <> name <> "-r1cs.jsonl"

mkWitnessFilePath :: FilePath -> String -> FilePath
mkWitnessFilePath rootDir name = rootDir <> "/" <> name <> "-witness.jsonl"

mkInputsFilePath :: FilePath -> String -> FilePath
mkInputsFilePath rootDir name = rootDir <> "/" <> name <> "-inputs.jsonl"
