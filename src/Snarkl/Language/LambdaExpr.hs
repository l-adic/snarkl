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
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Language.Core (Variable)
import Snarkl.Language.Expr (expSeq)
import qualified Snarkl.Language.Expr as E

-- This expression language is just the untyped version of the typed
-- expression language TEExp. It is used to remove lamba application
-- and abstraction before passing on to the next expression language.
-- There is also a certain amount of "flattening" between this representation
-- and the underlying Expr language -- we reassociate all of the Seq constructors
-- to the right and then flatten them. Similarly nested Binops of the same
-- operator are flattened into a single list if that operator is associative.
data Exp :: Type -> Type where
  EVar :: Variable -> Exp k
  EVal :: (GaloisField k) => k -> Exp k
  EUnop :: UnOp -> Exp k -> Exp k
  EBinop :: Op -> Exp k -> Exp k -> Exp k
  EIf :: Exp k -> Exp k -> Exp k -> Exp k
  EAssert :: Exp k -> Exp k -> Exp k
  ESeq :: Exp k -> Exp k -> Exp k
  EUnit :: Exp k
  EAbs :: Variable -> Exp k -> Exp k
  EApp :: Exp k -> Exp k -> Exp k

deriving instance Show (Exp k)

deriving instance Eq (Exp k)

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

expOfLambdaExp :: Exp k -> E.Exp k
expOfLambdaExp _exp =
  let coreExp = betaNormalize _exp
   in case expOfLambdaExp' coreExp of
        Left err -> failWith $ ErrMsg err
        Right e -> e
  where
    expOfLambdaExp' :: Exp k -> Either String (E.Exp k)
    expOfLambdaExp' = \case
      EVar var -> pure $ E.EVar var
      EVal v -> pure $ E.EVal v
      EUnit -> pure E.EUnit
      EUnop op e -> E.EUnop op <$> expOfLambdaExp' e
      EBinop op l r -> E.expBinop op <$> expOfLambdaExp' l <*> expOfLambdaExp' r
      EIf b e1 e2 -> E.EIf <$> expOfLambdaExp' b <*> expOfLambdaExp' e1 <*> expOfLambdaExp' e2
      EAssert e1 e2 -> E.EAssert <$> expOfLambdaExp' e1 <*> expOfLambdaExp' e2
      ESeq e1 e2 -> expSeq <$> expOfLambdaExp' e1 <*> expOfLambdaExp' e2
      e -> throwError ("Impossible after lambda simplicifaction: " <> show e)
