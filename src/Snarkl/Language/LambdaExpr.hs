{-# LANGUAGE LambdaCase #-}

module Snarkl.Language.LambdaExpr
  ( Exp (..),
    expOfLambdaExp,
  )
where

import Control.Monad.Error.Class (throwError)
import Data.Field.Galois (GaloisField)
import Data.Kind (Type)
import Snarkl.Common (Op, UnOp)
import Snarkl.Language.Expr (Variable)
import qualified Snarkl.Language.Expr as Core

data Exp :: Type -> Type where
  EVar :: Variable -> Exp a
  EVal :: (GaloisField a) => a -> Exp a
  EUnop :: UnOp -> Exp a -> Exp a
  EBinop :: Op -> Exp a -> Exp a -> Exp a
  EIf :: Exp a -> Exp a -> Exp a -> Exp a
  EAssert :: Exp a -> Exp a -> Exp a
  ESeq :: Exp a -> Exp a -> Exp a
  EUnit :: Exp a
  EAbs :: Variable -> Exp a -> Exp a
  EApp :: Exp a -> Exp a -> Exp a

deriving instance (Show a) => Show (Exp a)

deriving instance (Eq a) => Eq (Exp a)

betaNormalize :: Exp k -> Exp k
betaNormalize = \case
  EVar x -> EVar x
  EVal v -> EVal v
  EUnop op e -> EUnop op (betaNormalize e)
  EBinop op l r -> EBinop op (betaNormalize l) (betaNormalize r)
  EIf e1 e2 e3 -> EIf (betaNormalize e1) (betaNormalize e2) (betaNormalize e3)
  EAssert e1 e2 -> EAssert (betaNormalize e1) (betaNormalize e2)
  ESeq e1 e2 -> ESeq (betaNormalize e1) (betaNormalize e2)
  EAbs v e -> EAbs v (betaNormalize e)
  EApp f a ->
    case betaNormalize f of
      EAbs v e -> substitute (v, betaNormalize a) e
      f' -> EApp f' (betaNormalize a)
  EUnit -> EUnit
  where
    -- substitute x e1 e2 = e2 [x := e1 ]
    substitute :: (Variable, Exp k) -> Exp k -> Exp k
    substitute (var, e1) = \case
      e@(EVar var') -> if var == var' then e1 else e
      e@(EVal _) -> e
      EUnit -> EUnit
      EUnop op e -> EUnop op (substitute (var, e1) e)
      EBinop op l r -> EBinop op (substitute (var, e1) l) (substitute (var, e1) r)
      EIf b e2 e3 -> EIf (substitute (var, e1) b) (substitute (var, e1) e2) (substitute (var, e1) e3)
      EAssert e2 e3 -> EAssert (substitute (var, e1) e2) (substitute (var, e1) e3)
      ESeq l r -> ESeq (substitute (var, e1) l) (substitute (var, e1) r)
      EAbs var' e -> EAbs var' (substitute (var, e1) e)
      EApp e2 e3 -> EApp (substitute (var, e1) e2) (substitute (var, e1) e3)

expOfLambdaExp :: (Show a) => Exp a -> Core.Exp a
expOfLambdaExp _exp =
  let coreExp = betaNormalize _exp
   in case expOfLambdaExp' coreExp of
        Left err -> error err
        Right e -> e
  where
    expOfLambdaExp' :: (Show a) => Exp a -> Either String (Core.Exp a)
    expOfLambdaExp' = \case
      EVar var -> pure $ Core.EVar var
      EVal v -> pure $ Core.EVal v
      EUnit -> pure Core.EUnit
      EUnop op e -> Core.EUnop op <$> expOfLambdaExp' e
      EBinop op e1 e2 -> Core.exp_binop op <$> expOfLambdaExp' e1 <*> expOfLambdaExp' e2
      EIf b e1 e2 -> Core.EIf <$> expOfLambdaExp' b <*> expOfLambdaExp' e1 <*> expOfLambdaExp' e2
      EAssert e1 e2 -> Core.EAssert <$> expOfLambdaExp' e1 <*> expOfLambdaExp' e2
      ESeq e1 e2 -> Core.exp_seq <$> expOfLambdaExp' e1 <*> expOfLambdaExp' e2
      e -> throwError ("Impossible after IR simplicifaction: " <> show e)
