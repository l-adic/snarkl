{-# LANGUAGE PatternSynonyms #-}

module Snarkl.Language.Expr
  ( Exp (..),
    mkProgram,
    expSeq,
    expBinop,
  )
where

import Control.Error (hoistEither, runExceptT)
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
  )
import Control.Monad.State (State, evalState, gets, modify, runState)
import Data.Field.Galois (GaloisField)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq, fromList, (<|), (><), (|>), pattern Empty, pattern (:<|))
import Snarkl.Common (Op, UnOp, isAssoc)
import qualified Snarkl.Language.Core as Core
import Text.PrettyPrint.Leijen.Text
  ( Pretty (pretty),
    hsep,
    parens,
    punctuate,
    (<+>),
  )

data Exp :: Type -> Type where
  EVar :: Core.Variable -> Exp k
  EVal :: (GaloisField k) => k -> Exp k
  EUnop :: UnOp -> Exp k -> Exp k
  EBinop :: Op -> [Exp k] -> Exp k
  EIf :: Exp k -> Exp k -> Exp k -> Exp k
  EAssert :: Exp k -> Exp k -> Exp k
  ESeq :: Seq (Exp k) -> Exp k
  EUnit :: Exp k

deriving instance Eq (Exp k)

deriving instance Show (Exp k)

const_prop :: (GaloisField k) => Exp k -> State (Map Core.Variable k) (Exp k)
const_prop e =
  case e of
    EVar x -> lookup_var x
    EVal _ -> return e
    EUnop op e1 ->
      do
        e1' <- const_prop e1
        return $ EUnop op e1'
    EBinop op es ->
      do
        es' <- mapM const_prop es
        return $ EBinop op es'
    EIf e1 e2 e3 ->
      do
        e1' <- const_prop e1
        e2' <- const_prop e2
        e3' <- const_prop e3
        return $ EIf e1' e2' e3'
    EAssert (EVar x) (EVal c) -> add_bind (x, c)
    EAssert e1 e2 ->
      do
        e1' <- const_prop e1
        e2' <- const_prop e2
        return $ EAssert e1' e2'
    ESeq es ->
      do
        es' <- mapM const_prop es
        return $ ESeq es'
    EUnit -> return EUnit
  where
    lookup_var :: (GaloisField k) => Core.Variable -> State (Map Core.Variable k) (Exp k)
    lookup_var x0 =
      gets
        ( \m -> case Map.lookup x0 m of
            Nothing -> EVar x0
            Just c -> EVal c
        )
    add_bind :: (Core.Variable, k) -> State (Map Core.Variable k) (Exp k)
    add_bind (x0, c0) =
      do
        modify (Map.insert x0 c0)
        return EUnit

do_const_prop :: (GaloisField k) => Exp k -> Exp k
do_const_prop e = evalState (const_prop e) Map.empty

instance Pretty (Exp k) where
  pretty (EVar x) = "var_" <> pretty x
  pretty (EVal c) = pretty c
  pretty (EUnop op e1) = pretty op <> parens (pretty e1)
  pretty (EBinop op es) =
    foldl (\a b -> a <+> pretty op <+> pretty b) mempty es
  pretty (EIf b e1 e2) =
    "if" <+> pretty b <+> "then" <+> pretty e1 <+> "else" <+> pretty e2
  pretty (EAssert e1 e2) = pretty e1 <+> ":=" <+> pretty e2
  pretty (ESeq es) = parens $ hsep $ punctuate ";" $ map pretty (toList es)
  pretty EUnit = "()"

mkExpression :: Exp k -> Either String (Core.Exp k)
mkExpression = \case
  EVar x -> pure $ Core.EVar x
  EVal v -> pure $ Core.EVal v
  EUnop op e -> Core.EUnop op <$> mkExpression e
  EBinop op es -> Core.EBinop op <$> traverse mkExpression es
  EIf e1 e2 e3 -> Core.EIf <$> mkExpression e1 <*> mkExpression e2 <*> mkExpression e3
  EUnit -> pure Core.EUnit
  e -> throwError $ "mkExpression: " ++ show e

-- | Smart constructor for sequence, ensuring all expressions are
--  flattened to top level.
expSeq :: Exp k -> Exp k -> Exp k
expSeq e1 e2 =
  case (e1, e2) of
    (ESeq l1, ESeq l2) -> ESeq (l1 >< l2)
    (ESeq l1, _) -> ESeq (l1 |> e2)
    (_, ESeq l2) -> ESeq (e1 <| l2)
    (_, _) -> ESeq (fromList [e1, e2])

expBinop :: Op -> Exp k -> Exp k -> Exp k
expBinop op e1 e2 =
  case (e1, e2) of
    (EBinop op1 l1, EBinop op2 l2)
      | op1 == op2 && op2 == op && isAssoc op ->
          EBinop op (l1 ++ l2)
    (EBinop op1 l1, _)
      | op1 == op && isAssoc op ->
          EBinop op (l1 ++ [e2])
    (_, EBinop op2 l2)
      | op2 == op && isAssoc op ->
          EBinop op (e1 : l2)
    (_, _) -> EBinop op [e1, e2]

mkAssignment :: Exp k -> Either String (Core.Assignment k)
mkAssignment (EAssert (EVar v) e) = Core.Assignment v <$> mkExpression e
mkAssignment e = throwError $ "mkAssignment: expected EAssert, got " <> show e

-- At this point the expression should be either:
-- 1. A sequence of assignments, followed by an expression
-- 2. An expression
mkProgram :: (GaloisField k) => Exp k -> Either String (Core.Program k)
mkProgram _exp = do
  let e' = do_const_prop _exp
  case e' of
    ESeq es -> do
      let (eexpr, assignments) = runState (runExceptT $ go es) mempty
      Core.Program assignments <$> eexpr
      where
        go :: Seq (Exp k) -> ExceptT String (State (Seq (Core.Assignment k))) (Core.Exp k)
        go = \case
          Empty -> throwError "mkProgram: empty sequence"
          e :<| Empty -> hoistEither $ mkExpression e
          e :<| rest -> do
            case e of
              EUnit -> go rest
              _ -> do
                assignment <- hoistEither $ mkAssignment e
                modify (|> assignment)
                go rest
    _ -> Core.Program Empty <$> mkExpression e'
