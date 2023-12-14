{-# LANGUAGE UndecidableInstances #-}

module Snarkl.TExpr
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
    booleanVarsOfTexp,
    varOfTExp,
    locOfTexp,
    teSeq,
    lastSeq,
    expOfTExp,
  )
where

import Data.Kind (Type)
import Data.Typeable (Proxy (..), Typeable, typeOf, typeRep)
import Prettyprinter (Pretty (pretty), parens, (<+>))
import Snarkl.Common (Op, UnOp, Var)
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Expr
  ( Exp (EAssert, EIf, EUnit, EUnop, EVal, EVar),
    exp_binop,
    exp_seq,
  )
import Snarkl.Field (Field (one, zero))

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
  deriving (Typeable)

instance Pretty Ty where
  pretty ty = case ty of
    TField -> "Field"
    TBool -> "Bool"
    TArr _ty -> "Array" <+> pretty _ty
    TProd ty1 ty2 -> parens (pretty ty1 <+> "⨉" <+> pretty ty2)
    TSum ty1 ty2 -> parens (pretty ty1 <+> "+" <+> pretty ty2)
    TMu f -> "μ" <> parens (pretty f)
    TUnit -> "()"

type family Rep (f :: TFunct) (x :: Ty) :: Ty

type instance Rep ('TFConst ty) x = ty

type instance Rep 'TFId x = x

type instance Rep ('TFProd f g) x = 'TProd (Rep f x) (Rep g x)

type instance Rep ('TFSum f g) x = 'TSum (Rep f x) (Rep g x)

type instance Rep ('TFComp f g) x = Rep f (Rep g x)

newtype TVar (ty :: Ty) = TVar Var deriving (Eq, Show)

instance Pretty (TVar ty) where
  pretty (TVar x) = "var_" <+> pretty x

varIsBoolean :: (Typeable ty) => TVar ty -> Bool
varIsBoolean x =
  typeOf x == typeRep (Proxy :: Proxy (TVar 'TBool))

type Loc = Int

newtype TLoc (ty :: Ty) = TLoc Loc deriving (Eq, Show)

instance Pretty (TLoc ty) where
  pretty (TLoc x) = "loc_" <+> pretty x

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

deriving instance (Show a) => Show (TExp (b :: Ty) a)

-- instance (Eq a) => Eq (TExp (b :: Ty) a) where
--  TEVar x == TEVar y = x == y
--  TEVal a == TEVal b = a == b
--  TEUnop op e1 == TEUnop op' e1' =
--    op == op' && e1 == e1'
--  TEBinop (op :: TOp ty1 ty2 ty) (e1 :: TExp ty1 a) (e2 :: TExp ty2 a) == TEBinop (op' :: TOp ty1 ty2 ty) e1' e2' =
--    op == op' && e1 == e1' && e2 == e2'
--  TEIf e e1 e2 == TEIf e' e1' e2' =
--    e == e' && e1 == e1' && e2 == e2'
--  TEAssert e1 e2 == TEAssert e1' e2' =
--    e1 == e1' && e2 == e2'
--  TESeq e1 e2 == TESeq e1' e2' =
--    e1 == e1' && e2 == e2'
--  TEBot == TEBot = True
--  _ == _ = False

expOfVal :: (Field a) => Val ty a -> Exp a
expOfVal v = case v of
  VField c -> EVal c
  VTrue -> EVal one
  VFalse -> EVal zero
  VUnit -> EUnit
  VLoc l -> failWith $ ErrMsg $ "unresolved location " ++ show l

expOfTExp :: (Field a) => TExp ty a -> Exp a
expOfTExp te = case te of
  TEVar (TVar x) -> EVar x
  TEVal v -> expOfVal v
  TEUnop (TUnop op) te1 ->
    EUnop op (expOfTExp te1)
  TEBinop (TOp op) te1 te2 ->
    exp_binop op (expOfTExp te1) (expOfTExp te2)
  TEIf te1 te2 te3 ->
    EIf (expOfTExp te1) (expOfTExp te2) (expOfTExp te3)
  TEAssert te1 te2 ->
    EAssert (expOfTExp te1) (expOfTExp te2)
  TESeq te1 te2 -> exp_seq (expOfTExp te1) (expOfTExp te2)
  TEBot -> EUnit

-- | Smart constructor for 'TESeq'.  Simplify 'TESeq te1 te2' to 'te2'
-- whenever the normal form of 'te1' (with seq's reassociated right)
-- is *not* equal 'TEAssert _ _'.
teSeq :: (Typeable ty1) => TExp ty1 a -> TExp ty2 a -> TExp ty2 a
teSeq te1 te2 = case (te1, te2) of
  (TEAssert _ _, _) -> TESeq te1 te2
  (TESeq tx ty, _) -> teSeq tx (teSeq ty te2)
  (_, _) -> te2

booleanVarsOfTexp :: (Typeable ty) => TExp ty a -> [Var]
booleanVarsOfTexp = go []
  where
    go :: (Typeable ty) => [Var] -> TExp ty a -> [Var]
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

varOfTExp :: (Show (TExp ty a)) => TExp ty a -> Var
varOfTExp te = case lastSeq te of
  TEVar (TVar x) -> x
  _ -> failWith $ ErrMsg ("varOfTExp: expected var: " ++ show te)

locOfTexp :: (Show (TExp ty a)) => TExp ty a -> Var
locOfTexp te = case lastSeq te of
  TEVal (VLoc (TLoc l)) -> l
  _ -> failWith $ ErrMsg ("locOfTexp: expected loc: " ++ show te)

lastSeq :: TExp ty a -> TExp ty a
lastSeq te = case te of
  TESeq _ te2 -> lastSeq te2
  _ -> te

instance (Pretty a) => Pretty (TExp ty a) where
  pretty (TEVar var) = "Var" <+> pretty var
  pretty (TEVal val) = pretty val
  pretty (TEUnop unop _exp) = pretty unop <+> pretty _exp
  pretty (TEBinop binop exp1 exp2) = pretty exp1 <+> pretty binop <+> pretty exp2
  pretty (TEIf condExp thenExp elseExp) = "if" <+> pretty condExp <+> "then" <+> pretty thenExp <+> "else" <+> pretty elseExp
  pretty (TEAssert exp1 exp2) = pretty exp1 <+> ":=" <+> pretty exp2
  pretty (TESeq exp1 exp2) = parens (pretty exp1 <+> ";" <+> pretty exp2)
  pretty TEBot = "⊥"