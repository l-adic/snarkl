{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snarkl.Common where

import qualified Data.Aeson as A
import Data.Field.Galois (PrimeField (fromP))
import Data.Foldable (Foldable (toList))
import qualified Data.Map as Map
import Text.PrettyPrint.Leijen.Text (Pretty (pretty))

newtype Var = Var Int deriving (Eq, Ord, Show)

instance A.ToJSON Var where
  toJSON (Var i) = A.toJSON (i + 1)

instance A.FromJSON Var where
  parseJSON v = do
    i <- A.parseJSON v
    return $ Var (i - 1)

instance Pretty Var where
  pretty (Var i) = "x_" <> pretty i

incVar :: Var -> Var
incVar (Var i) = Var (i + 1)

newtype Assgn a = Assgn (Map.Map Var a) deriving (Show, Eq, Functor, Foldable)

newtype FieldElem k = FieldElem {unFieldElem :: k} deriving (Show, Eq)

instance (PrimeField k) => A.ToJSON (FieldElem k) where
  toJSON (FieldElem k) = A.toJSON (show $ fromP k)

instance (PrimeField k) => A.FromJSON (FieldElem k) where
  parseJSON v = do
    k <- A.parseJSON v
    return $ FieldElem (fromInteger $ read k)

instance (PrimeField a) => A.ToJSON (Assgn a) where
  toJSON (Assgn m) =
    let kvs = map (\(var, coeff) -> (FieldElem coeff, var)) $ Map.toList m
     in A.toJSON kvs

instance (PrimeField a) => A.FromJSON (Assgn a) where
  parseJSON =
    A.withArray "Assgn" $ \arr -> do
      kvs <- mapM A.parseJSON arr
      let m = Map.fromList $ map (\(FieldElem k, v) -> (v, k)) (toList kvs)
      return (Assgn m)

data UnOp = ZEq
  deriving (Eq, Show)

instance Pretty UnOp where
  pretty ZEq = "isZero"

data Op
  = Add
  | Sub
  | Mult
  | Div
  | And
  | Or
  | XOr
  | Eq
  | BEq
  deriving (Eq, Show)

instance Pretty Op where
  pretty Add = "+"
  pretty Sub = "-"
  pretty Mult = "*"
  pretty Div = "-*"
  pretty And = "&&"
  pretty Or = "||"
  pretty XOr = "xor"
  pretty Eq = "=="
  pretty BEq = "=b="

isBoolean :: Op -> Bool
isBoolean op = case op of
  Add -> False
  Sub -> False
  Mult -> False
  Div -> False
  And -> True
  Or -> True
  XOr -> True
  Eq -> True
  BEq -> True

isAssoc :: Op -> Bool
isAssoc op = case op of
  Add -> True
  Sub -> False
  Mult -> True
  Div -> False
  And -> True
  Or -> True
  XOr -> True
  Eq -> True
  BEq -> True

data ConstraintHeader = ConstraintHeader
  { field_characteristic :: Integer,
    extension_degree :: Integer,
    n_constraints :: Int,
    n_variables :: Int,
    input_variables :: [Var],
    output_variables :: [Var]
  }

instance A.ToJSON ConstraintHeader where
  toJSON (ConstraintHeader {..}) =
    A.object
      [ "field_characteristic" A..= field_characteristic,
        "extension_degree" A..= extension_degree,
        "n_constraints" A..= n_constraints,
        "n_variables" A..= n_variables,
        "input_variables" A..= input_variables,
        "output_variables" A..= output_variables
      ]

instance A.FromJSON ConstraintHeader where
  parseJSON =
    A.withObject "ConstraintHeader" $ \v -> do
      field_characteristic <- v A..: "field_characteristic"
      extension_degree <- v A..: "extension_degree"
      n_constraints <- v A..: "n_constraints"
      n_variables <- v A..: "n_variables"
      input_variables <- v A..: "input_variables"
      output_variables <- v A..: "output_variables"
      pure $
        ConstraintHeader
          { field_characteristic,
            extension_degree,
            n_constraints,
            n_variables,
            input_variables,
            output_variables
          }
