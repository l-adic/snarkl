{-# LANGUAGE PatternSynonyms #-}

module Snarkl.Language.Expr
  ( Exp (..),
    mkProgram',
    expSeq,
    expBinop,
    do_const_prop,
  )
where

import Control.Error (runExceptT)
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
import Data.Sequence (Seq, filter, fromList, singleton, (<|), (><), (|>), pattern Empty, pattern (:<|))
import Snarkl.Common (Op, UnOp, isAssoc)
import qualified Snarkl.Language.Core as Core
import Text.PrettyPrint.Leijen.Text
  ( Pretty (pretty),
    hsep,
    parens,
    punctuate,
    (<+>),
  )
import Prelude hiding (filter)

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

mkExpression ::
  (GaloisField k) => Exp k -> ExceptT String (State (Seq (Core.Assignment k))) (Core.Exp k)
mkExpression = \case
  EVar x -> pure $ Core.EVar x
  EVal v -> pure $ Core.EVal v
  EUnop op e -> Core.EUnop op <$> mkProgram e
  EBinop op es -> Core.EBinop op <$> traverse mkProgram es
  EIf e1 e2 e3 -> Core.EIf <$> mkProgram e1 <*> mkProgram e2 <*> mkProgram e3
  EUnit -> pure Core.EUnit
  ESeq es -> do
    res <- traverse mkProgram es
    case filter (/= Core.EUnit) res of
      Empty -> throwError "Fuck"
      e :<| Empty -> pure e
      e :<| _ -> throwError $ "something went wrong in mkExpression: " <> show e
  EAssert (EVar v) e -> do
    e' <- mkProgram e
    modify (<> singleton (Core.Assignment v e'))
    pure Core.EUnit
  EAssert {} -> throwError "something went wrong in mkExpression: EAssert"

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

-- At this point the expression should be either:
-- 1. A sequence of assignments, followed by an expression
-- 2. An expression
mkProgram ::
  (GaloisField k) =>
  Exp k ->
  ExceptT String (State (Seq (Core.Assignment k))) (Core.Exp k)
mkProgram _exp = do
  case _exp of
    ESeq es -> case es of
      Empty -> throwError "mkProgram: empty sequence"
      e :<| Empty -> mkProgram e
      e :<| rest -> do
        case e of
          EUnit -> mkProgram $ ESeq rest
          EAssert (EVar v) e' -> do
            res <- mkProgram e'
            modify (<> singleton (Core.Assignment v res))
            mkProgram $ ESeq rest
          _ -> throwError "mkProgram: expected EUnit or EAssert"
    e -> mkExpression e

-- showConstructor :: Exp k -> String
-- showConstructor = \case
--  EVar {} -> "EVar"
--  EVal {} -> "EVal"
--  EUnop {} -> "EUnop"
--  EBinop {} -> "EBinop"
--  EIf {} -> "EIf"
--  EAssert {} -> "EAssert"
--  ESeq {} -> "ESeq"
--  EUnit {} -> "EUnit"

mkProgram' :: (GaloisField k) => Exp k -> Either String (Core.Program k)
mkProgram' _exp = do
  let (res, assigns) = runState (runExceptT $ mkProgram $ do_const_prop _exp) Empty
  case res of
    Left err -> Left err
    Right e -> Right $ Core.Program assigns e
