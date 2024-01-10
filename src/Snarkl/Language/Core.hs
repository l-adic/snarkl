module Snarkl.Language.Core where

import Data.Field.Galois (GaloisField)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Sequence (Seq)
import Snarkl.Common (Op, UnOp)
import Text.PrettyPrint.Leijen.Text (Pretty (..), hsep, line, parens, punctuate, (<+>))

newtype Variable = Variable Int deriving (Eq, Ord, Show)

instance Pretty Variable where
  pretty (Variable i) = "x_" <> pretty i

data Exp :: Type -> Type where
  EVar :: Variable -> Exp k
  EVal :: (GaloisField k) => k -> Exp k
  EUnop :: UnOp -> Exp k -> Exp k
  EBinop :: Op -> [Exp k] -> Exp k
  EIf :: Exp k -> Exp k -> Exp k -> Exp k
  EUnit :: Exp k

deriving instance Eq (Exp k)

deriving instance Show (Exp k)

instance Pretty (Exp k) where
  pretty (EVar v) = pretty v
  pretty (EVal v) = pretty v
  pretty (EUnop op e) = pretty op <+> pretty e
  pretty (EBinop op es) = parens $ hsep $ punctuate (pretty op) $ map pretty es
  pretty (EIf e1 e2 e3) = parens $ hsep ["if", pretty e1, pretty e2, pretty e3]
  pretty EUnit = "()"

data Assignment a = Assignment Variable (Exp a)

instance Pretty (Assignment k) where
  pretty (Assignment v e) = pretty v <+> ":=" <+> pretty e

data Program :: Type -> Type where
  Program :: Seq (Assignment a) -> Exp a -> Program a

assignments :: Program a -> Seq (Assignment a)
assignments (Program as _) = as

result :: Program a -> Exp a
result (Program _ e) = e

instance Pretty (Program k) where
  pretty (Program as e) = mconcat (intercalate [line] (map (\a -> [pretty a]) $ toList as)) <> line <> pretty e
