module Snarkl.Language.Core where

import Data.Field.Galois (GaloisField)
import Data.Kind (Type)
import Data.Sequence (Seq)
import Snarkl.Common (Op, UnOp)
import Text.PrettyPrint.Leijen.Text (Pretty)

newtype Variable = Variable Int deriving (Eq, Ord, Show, Pretty)

data Exp :: Type -> Type where
  EVar :: Variable -> Exp k
  EVal :: (GaloisField k) => k -> Exp k
  EUnop :: UnOp -> Exp k -> Exp k
  EBinop :: Op -> [Exp k] -> Exp k
  EIf :: Exp k -> Exp k -> Exp k -> Exp k
  EUnit :: Exp k

deriving instance Eq (Exp k)

deriving instance Show (Exp k)

data Assignment a = Assignment Variable (Exp a)

data Program :: Type -> Type where
  Program :: Seq (Assignment a) -> Exp a -> Program a
