{-# LANGUAGE LambdaCase #-}

module Snarkl.Language.Core where

import Data.Field.Galois (GaloisField)
import Data.Kind (Type)
import Prettyprinter (Pretty)
import Snarkl.Common

newtype Variable = Variable Int deriving (Eq, Ord, Show, Pretty)

data Exp :: Type -> Type where
  EVar :: Variable -> Exp a
  EVal :: (GaloisField a) => a -> Exp a
  EUnop :: UnOp -> Exp a -> Exp a
  EBinop :: Op -> [Exp a] -> Exp a
  EIf :: Exp a -> Exp a -> Exp a -> Exp a
  EUnit :: Exp a

deriving instance (Eq a) => Eq (Exp a)

deriving instance (Show a) => Show (Exp a)

data Assignment a = Assignment Variable (Exp a)

data Program :: Type -> Type where
  Program :: [Assignment a] -> Exp a -> Program a