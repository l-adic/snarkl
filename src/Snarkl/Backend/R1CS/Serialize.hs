{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Snarkl.Backend.R1CS.Serialize where

import Control.Monad.Except (throwError)
import qualified Data.Aeson as A
import Data.ByteString.Builder (Builder, lazyByteString, toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Field.Galois (GaloisField (char, deg), PrimeField, fromP)
import Data.List (sortOn)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Snarkl.Backend.R1CS.R1CS (R1CS (..), num_constraints)
import Snarkl.Common (Assgn, Var)
import Prelude hiding (writeFile)

data R1CSHeader k = R1CSHeader
  { field_characteristic :: Integer,
    extension_degree :: Integer,
    n_constraints :: Int,
    n_variables :: Int,
    input_variables :: [Var],
    output_variables :: [Var]
  }
  deriving (Generic)

instance A.ToJSON (R1CSHeader k)

instance A.FromJSON (R1CSHeader k)

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
   in toLazyByteString (unJSONLine b)

deserializeR1CS :: (PrimeField k) => LBS.ByteString -> Either String (R1CS k)
deserializeR1CS file = do
  let ls = LBS.split 0x0a file
  case ls of
    [] -> throwError "Empty file"
    (h : cs) -> do
      header <- A.eitherDecode h
      clauses <- mapM A.eitherDecode cs
      return $
        R1CS
          { r1cs_clauses = clauses,
            r1cs_num_vars = n_variables header,
            r1cs_in_vars = input_variables header,
            r1cs_out_vars = output_variables header
          }

serializeWitnessAsJson :: (PrimeField k) => R1CSHeader k -> Assgn k -> LBS.ByteString
serializeWitnessAsJson header assgn =
  let inputs_assgn = map (\(v, f) -> (v, show $ fromP f)) $ Map.toAscList assgn
      b = jsonLine header <> jsonlBuilder inputs_assgn
   in toLazyByteString $ unJSONLine b

serializeInputsAsJson :: (PrimeField k) => R1CS k -> [k] -> LBS.ByteString
serializeInputsAsJson r1cs inputs =
  let inputs_assgn =
        map (\(v, f) -> (v, show $ fromP f)) $
          sortOn fst $
            zip (r1cs_in_vars r1cs) inputs
   in toLazyByteString $ unJSONLine $ jsonlBuilder inputs_assgn

mkR1CSFilePath :: FilePath -> String -> FilePath
mkR1CSFilePath rootDir name = rootDir <> "/" <> name <> "-r1cs.jsonl"

mkWitnessFilePath :: FilePath -> String -> FilePath
mkWitnessFilePath rootDir name = rootDir <> "/" <> name <> "-witness.jsonl"

mkInputsFilePath :: FilePath -> String -> FilePath
mkInputsFilePath rootDir name = rootDir <> "/" <> name <> "-inputs.jsonl"

parseInputs :: (PrimeField k) => LBS.ByteString -> Either String [k]
parseInputs file =
  map fromInteger <$> do
    let ls = LBS.split 0x0a file
    case ls of
      [] -> pure ([] :: [Integer])
      inputs -> traverse A.eitherDecode inputs

newtype JSONLine = JSONLine {unJSONLine :: Builder}

instance Semigroup JSONLine where
  JSONLine a <> JSONLine b = JSONLine (a <> "\n" <> b)

jsonLine :: (A.ToJSON a) => a -> JSONLine
jsonLine = JSONLine . lazyByteString . A.encode

jsonlBuilder :: (Foldable t, Functor t, A.ToJSON a) => t a -> JSONLine
jsonlBuilder = foldl1 (<>) . fmap jsonLine