{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snarkl.Common where

import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Map as Map
import Data.Ratio
import Prettyprinter

type Var = Int

type Assgn a = IntMap.IntMap a

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

instance Pretty Rational where
  pretty r
    | denominator r == 1 = pretty (numerator r)
    | otherwise = pretty (numerator r) <> "/" <> pretty (denominator r)

intMapToMap :: IntMap.IntMap v -> Map.Map Int v
intMapToMap = Map.fromList . IntMap.toList
