module Snarkl.AST.Type where

import Data.Nat (Nat)
import Data.Typeable (Typeable)
import Text.PrettyPrint.Leijen.Text (Pretty (pretty), parens, (<+>))

data TFunct where
  TFConst :: Ty -> TFunct
  TFId :: TFunct
  TFProd :: TFunct -> TFunct -> TFunct
  TFSum :: TFunct -> TFunct -> TFunct
  TFComp :: TFunct -> TFunct -> TFunct
  deriving (Typeable)

instance Pretty TFunct where
  pretty f = case f of
    TFConst ty -> "Const" <+> pretty ty
    TFId -> "Id"
    TFProd f1 f2 -> parens (pretty f1 <+> "⊗" <+> pretty f2)
    TFSum f1 f2 -> parens (pretty f1 <+> "⊕" <+> pretty f2)
    TFComp f1 f2 -> parens (pretty f1 <+> "∘" <+> pretty f2)

data Ty where
  TField :: Ty
  TBool :: Ty
  TArr :: Ty -> Ty
  TVec :: Nat -> Ty -> Ty
  TProd :: Ty -> Ty -> Ty
  TSum :: Ty -> Ty -> Ty
  TMu :: TFunct -> Ty
  TUnit :: Ty
  TFun :: Ty -> Ty -> Ty
  deriving (Typeable)

deriving instance Typeable 'TField

deriving instance Typeable 'TBool

deriving instance Typeable 'TArr

deriving instance Typeable 'TVec

deriving instance Typeable 'TProd

deriving instance Typeable 'TSum

deriving instance Typeable 'TMu

deriving instance Typeable 'TUnit

deriving instance Typeable 'TFun

instance Pretty Ty where
  pretty ty = case ty of
    TField -> "Field"
    TBool -> "Bool"
    TArr _ty -> "Array" <+> pretty _ty
    TVec n _ty -> "Vec" <+> pretty (toInteger n) <+> pretty _ty
    TProd ty1 ty2 -> parens (pretty ty1 <+> "⨉" <+> pretty ty2)
    TSum ty1 ty2 -> parens (pretty ty1 <+> "+" <+> pretty ty2)
    TMu f -> "μ" <> parens (pretty f)
    TUnit -> "()"
    TFun ty1 ty2 -> parens (pretty ty1 <+> "->" <+> pretty ty2)

type family Rep (f :: TFunct) (x :: Ty) :: Ty

type instance Rep ('TFConst ty) x = ty

type instance Rep 'TFId x = x

type instance Rep ('TFProd f g) x = 'TProd (Rep f x) (Rep g x)

type instance Rep ('TFSum f g) x = 'TSum (Rep f x) (Rep g x)

type instance Rep ('TFComp f g) x = Rep f (Rep g x)
