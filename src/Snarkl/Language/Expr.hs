{-# LANGUAGE LambdaCase #-}

module Snarkl.Language.Expr
  ( Exp (..),
    var_of_exp,
    do_const_prop,
    mkProgram,
    expSeq,
  )
where

import Control.Error (hoistEither, runExceptT)
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    MonadPlus (mzero),
  )
import Control.Monad.State (State, evalState, gets, modify, runState)
import Data.Field.Galois (GaloisField)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq, (|>))
import Debug.Trace (trace)
import Prettyprinter
  ( Pretty (pretty),
    hsep,
    parens,
    punctuate,
    (<+>),
  )
import Snarkl.Common (Op, UnOp)
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import qualified Snarkl.Language.Core as Core

data Exp :: Type -> Type where
  EVar :: Core.Variable -> Exp a
  EVal :: (GaloisField a) => a -> Exp a
  EUnop :: UnOp -> Exp a -> Exp a
  EBinop :: Op -> [Exp a] -> Exp a
  EIf :: Exp a -> Exp a -> Exp a -> Exp a
  EAssert :: Exp a -> Exp a -> Exp a
  ESeq :: [Exp a] -> Exp a
  EUnit :: Exp a

deriving instance (Eq a) => Eq (Exp a)

deriving instance (Show a) => Show (Exp a)

var_of_exp :: (Show a) => Exp a -> Core.Variable
var_of_exp e = case e of
  EVar x -> x
  _ -> failWith $ ErrMsg ("var_of_exp: expected variable: " ++ show e)

const_prop :: (GaloisField a) => Exp a -> State (Map Core.Variable a) (Exp a)
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
    lookup_var :: (GaloisField a) => Core.Variable -> State (Map Core.Variable a) (Exp a)
    lookup_var x0 =
      gets
        ( \m -> case Map.lookup x0 m of
            Nothing -> EVar x0
            Just c -> EVal c
        )
    add_bind :: (Core.Variable, a) -> State (Map Core.Variable a) (Exp a)
    add_bind (x0, c0) =
      do
        modify (Map.insert x0 c0)
        return EUnit

do_const_prop :: (GaloisField a) => Exp a -> Exp a
do_const_prop e = evalState (const_prop e) Map.empty

instance (Pretty a) => Pretty (Exp a) where
  pretty (EVar x) = "var_" <> pretty x
  pretty (EVal c) = pretty c
  pretty (EUnop op e1) = pretty op <> parens (pretty e1)
  pretty (EBinop op es) =
    foldl (\a b -> a <+> pretty op <+> pretty b) mempty es
  pretty (EIf b e1 e2) =
    "if" <+> pretty b <+> "then" <+> pretty e1 <+> "else" <+> pretty e2
  pretty (EAssert e1 e2) = pretty e1 <+> ":=" <+> pretty e2
  pretty (ESeq es) = parens $ hsep $ punctuate ";" $ map pretty es
  pretty EUnit = "()"

mkExpression :: (Show a) => Exp a -> Either String (Core.Exp a)
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
expSeq :: Exp a -> Exp a -> Exp a
expSeq e1 e2 =
  case (e1, e2) of
    (ESeq l1, ESeq l2) -> ESeq (l1 ++ l2)
    (ESeq l1, _) -> ESeq (l1 ++ [e2])
    (_, ESeq l2) -> ESeq (e1 : l2)
    (_, _) -> ESeq [e1, e2]

mkAssignment :: (Show a) => Exp a -> Either String (Core.Assignment a)
mkAssignment (EAssert (EVar v) e) = Core.Assignment v <$> mkExpression e
mkAssignment e = throwError $ "mkAssignment: expected EAssert, got " <> show e

mkProgram :: (Show a) => Exp a -> Either String (Core.Program a)
mkProgram e@(ESeq es) = trace ("mkProgram ESeq: " <> show e) $ do
  let (eexpr, assignments) = runState (runExceptT $ go es) mempty
  Core.Program (toList assignments) <$> eexpr
  where
    go :: (Show a) => [Exp a] -> ExceptT String (State (Seq (Core.Assignment a))) (Core.Exp a)
    go = \case
      [] -> mzero
      [e] -> hoistEither $ mkExpression e
      e : rest -> do
        case e of
          EUnit -> go rest
          _ -> do
            assignment <- hoistEither $ mkAssignment e
            modify (|> assignment)
            go rest
mkProgram e = trace ("mkProgram " <> show e) $ do
  e' <- mkExpression e
  pure $ Core.Program [] e'