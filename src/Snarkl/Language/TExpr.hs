{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Snarkl.Language.TExpr
  ( Val (..),
    TExp (..),
    TFunct (..),
    Ty (..),
    Rep,
    TUnop (..),
    TOp (..),
    TVar (..),
    Loc,
    TLoc (..),
    Core.Variable (..),
    booleanVarsOfTexp,
    lambdaExpOfTExp,
    varOfTExp,
    locOfTexp,
    teSeq,
    lastSeq,
    -- expOfTExp,
  )
where

import Data.Kind (Type)
import Data.Typeable (Proxy (..), Typeable, eqT, typeOf, typeRep, type (:~:) (Refl))
import Prettyprinter (Pretty (pretty), line, parens, (<+>))
import Snarkl.Common (Op, UnOp)
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Field (Field (one, zero))
import qualified Snarkl.Language.Expr as Core
import qualified Snarkl.Language.LambdaExpr as LE

--  ( Exp (EAssert, EIf, EUnit, EUnop, EVal, EVar),
--    Variable (..),
--    exp_binop,
--    exp_seq,
--  )

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
  TProd :: Ty -> Ty -> Ty
  TSum :: Ty -> Ty -> Ty
  TMu :: TFunct -> Ty
  TUnit :: Ty
  TFun :: Ty -> Ty -> Ty
  deriving (Typeable)

deriving instance Typeable 'TField

deriving instance Typeable 'TBool

deriving instance Typeable 'TArr

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

newtype TVar (ty :: Ty) = TVar Core.Variable deriving (Eq, Show)

instance Pretty (TVar ty) where
  pretty (TVar x) = "var_" <> pretty x

varIsBoolean :: (Typeable ty) => TVar ty -> Bool
varIsBoolean x =
  typeOf x == typeRep (Proxy :: Proxy (TVar 'TBool))

type Loc = Int

newtype TLoc (ty :: Ty) = TLoc Loc deriving (Eq, Show)

instance Pretty (TLoc ty) where
  pretty (TLoc x) = "loc_" <> pretty x

data TUnop :: Ty -> Ty -> Type where
  TUnop :: forall ty1 ty. UnOp -> TUnop ty1 ty
  deriving (Eq, Show)

instance Pretty (TUnop ty1 ty) where
  pretty (TUnop op) = pretty op

data TOp :: Ty -> Ty -> Ty -> Type where
  TOp :: forall ty1 ty2 ty. Op -> TOp ty1 ty2 ty
  deriving (Eq, Show)

instance Pretty (TOp ty1 ty2 ty3) where
  pretty (TOp op) = pretty op

data Val :: Ty -> Type -> Type where
  VField :: (Field a) => a -> Val 'TField a
  VTrue :: Val 'TBool a
  VFalse :: Val 'TBool a
  VUnit :: Val 'TUnit a
  VLoc :: TLoc ty -> Val ty a

deriving instance (Eq a) => Eq (Val (b :: Ty) a)

deriving instance (Show a) => Show (Val (b :: Ty) a)

instance (Pretty a) => Pretty (Val ty a) where
  pretty v = case v of
    VField a -> pretty a
    VTrue -> "true"
    VFalse -> "false"
    VUnit -> "()"
    VLoc l -> pretty l

data TExp :: Ty -> Type -> Type where
  TEVar :: TVar ty -> TExp ty a
  TEVal :: Val ty a -> TExp ty a
  TEUnop ::
    ( Typeable ty1
    ) =>
    TUnop ty1 ty ->
    TExp ty1 a ->
    TExp ty a
  TEBinop ::
    ( Typeable ty1,
      Typeable ty2
    ) =>
    TOp ty1 ty2 ty ->
    TExp ty1 a ->
    TExp ty2 a ->
    TExp ty a
  TEIf :: TExp 'TBool a -> TExp ty a -> TExp ty a -> TExp ty a
  TEAssert :: (Typeable ty) => TExp ty a -> TExp ty a -> TExp 'TUnit a
  TESeq :: TExp 'TUnit a -> TExp ty2 a -> TExp ty2 a
  TEBot :: (Typeable ty) => TExp ty a
  TEAbs :: (Typeable ty, Typeable ty1) => TVar ty -> TExp ty1 a -> TExp ('TFun ty ty1) a
  TEApp :: (Typeable ty, Typeable ty1) => TExp ('TFun ty ty1) a -> TExp ty a -> TExp ty1 a

deriving instance (Show a) => Show (TExp (b :: Ty) a)

instance (Eq a) => Eq (TExp (b :: Ty) a) where
  TEVar x == TEVar y = x == y
  TEVal a == TEVal b = a == b
  TEUnop (op :: TUnop ty1 ty) e1 == TEUnop (op' :: TUnop ty2 ty) e1' =
    case eqT @ty1 @ty2 of
      Just Refl -> op == op' && e1 == e1'
      Nothing -> False
  TEBinop (op :: TOp ty1 ty2 ty) (e1 :: TExp ty1 a) (e2 :: TExp ty2 a) == TEBinop (op' :: TOp ty3 ty4 ty) e1' e2' =
    case (eqT @ty1 @ty3, eqT @ty2 @ty4) of
      (Just Refl, Just Refl) -> op == op' && e1 == e1' && e2 == e2'
      _ -> False
  TEIf e e1 e2 == TEIf e' e1' e2' =
    e == e' && e1 == e1' && e2 == e2'
  TEAssert (e1 :: TExp ty1 a) (e2 :: TExp ty1 a) == TEAssert (e1' :: TExp ty2 a) (e2' :: TExp ty2 a) =
    case eqT @ty1 @ty2 of
      Just Refl -> e1 == e1' && e2 == e2'
      Nothing -> False
  TESeq e1 e2 == TESeq e1' e2' =
    e1 == e1' && e2 == e2'
  TEBot == TEBot = True
  _ == _ = False

lambdaExpOfTExp :: (Field a, Typeable ty) => TExp ty a -> LE.Exp a
lambdaExpOfTExp te = case te of
  TEVar (TVar x) -> LE.EVar x
  TEVal v -> lambdaExpOfVal v
  TEUnop (TUnop op) te1 ->
    LE.EUnop op (lambdaExpOfTExp te1)
  TEBinop (TOp op) te1 te2 ->
    LE.expBinop op (lambdaExpOfTExp te1) (lambdaExpOfTExp te2)
  TEIf te1 te2 te3 ->
    LE.EIf (lambdaExpOfTExp te1) (lambdaExpOfTExp te2) (lambdaExpOfTExp te3)
  TEAssert te1 te2 ->
    LE.EAssert (lambdaExpOfTExp te1) (lambdaExpOfTExp te2)
  TESeq te1 te2 -> LE.ESeq (lambdaExpOfTExp te1) (lambdaExpOfTExp te2)
  TEBot -> LE.EUnit
  TEAbs (TVar v) e -> LE.EAbs v (lambdaExpOfTExp e)
  TEApp e1 e2 -> LE.EApp (lambdaExpOfTExp e1) (lambdaExpOfTExp e2)
  where
    lambdaExpOfVal :: (Field a) => Val ty a -> LE.Exp a
    lambdaExpOfVal v = case v of
      VField c -> LE.EVal c
      VTrue -> LE.EVal one
      VFalse -> LE.EVal zero
      VUnit -> LE.EUnit
      VLoc l -> failWith $ ErrMsg $ "unresolved location " ++ show l

-- | Smart constructor for 'TESeq'.  Simplify 'TESeq te1 te2' to 'te2'
-- whenever the normal form of 'te1' (with seq's reassociated right)
-- is *not* equal 'TEAssert _ _'.
teSeq :: (Typeable ty1) => TExp ty1 a -> TExp ty2 a -> TExp ty2 a
teSeq te1 te2 = case (te1, te2) of
  (TEAssert _ _, _) -> TESeq te1 te2
  (TESeq tx ty, _) -> teSeq tx (teSeq ty te2)
  (_, _) -> te2

booleanVarsOfTexp :: (Typeable ty) => TExp ty a -> [Core.Variable]
booleanVarsOfTexp = go []
  where
    go :: (Typeable ty) => [Core.Variable] -> TExp ty a -> [Core.Variable]
    go vars (TEVar t@(TVar x)) =
      if varIsBoolean t
        then x : vars
        else vars
    go vars (TEVal _) = vars
    go vars (TEUnop _ e1) = go vars e1
    go vars (TEBinop _ e1 e2) = go (go vars e1) e2
    go vars (TEIf e1 e2 e3) =
      go (go (go vars e1) e2) e3
    go vars (TEAssert e1 e2) = go (go vars e1) e2
    go vars (TESeq e1 e2) = go (go vars e1) e2
    go vars TEBot = vars
    go vars (TEAbs _ e) = go vars e
    go vars (TEApp e1 e2) = go (go vars e1) e2

varOfTExp :: (Show (TExp ty a)) => TExp ty a -> Core.Variable
varOfTExp te = case lastSeq te of
  TEVar (TVar x) -> x
  _ -> failWith $ ErrMsg ("varOfTExp: expected var: " ++ show te)

locOfTexp :: (Show (TExp ty a)) => TExp ty a -> Loc
locOfTexp te = case lastSeq te of
  TEVal (VLoc (TLoc l)) -> l
  _ -> failWith $ ErrMsg ("locOfTexp: expected loc: " ++ show te)

lastSeq :: TExp ty a -> TExp ty a
lastSeq te = case te of
  TESeq _ te2 -> lastSeq te2
  _ -> te

instance (Pretty a, Typeable ty) => Pretty (TExp ty a) where
  pretty (TEVar var) = pretty var
  pretty (TEVal val) = pretty val
  pretty (TEUnop unop _exp) = pretty unop <+> pretty _exp
  pretty (TEBinop binop exp1 exp2) = pretty exp1 <+> pretty binop <+> pretty exp2
  pretty (TEIf condExp thenExp elseExp) = "if" <+> pretty condExp <+> "then" <+> pretty thenExp <+> "else" <+> pretty elseExp
  pretty (TEAssert exp1 exp2) = pretty exp1 <+> ":=" <+> pretty exp2
  pretty (TESeq exp1 exp2) = parens (pretty exp1 <+> ";" <> line <> pretty exp2)
  pretty TEBot = "⊥"
  pretty (TEAbs var _exp) = parens ("\\" <> pretty var <+> "->" <+> pretty _exp)
  pretty (TEApp exp1 exp2) = parens (pretty exp1 <+> pretty exp2)
