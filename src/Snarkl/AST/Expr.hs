module Snarkl.AST.Expr
  ( Exp (..),
    Variable (..),
    exp_binop,
    exp_seq,
    is_pure,
    var_of_exp,
    do_const_prop,
  )
where

import Control.Monad.State (State, evalState, gets, modify)
import Data.Field.Galois (GaloisField)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq, fromList, (<|), (><), (|>))
import Snarkl.Common (Op, UnOp, isAssoc)
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Text.PrettyPrint.Leijen.Text
  ( Pretty (pretty),
    hsep,
    parens,
    punctuate,
    (<+>),
  )

newtype Variable = Variable Int deriving (Eq, Ord, Show, Pretty)

data Exp :: Type -> Type where
  EVar :: Variable -> Exp a
  EVal :: (GaloisField a) => a -> Exp a
  EUnop :: UnOp -> Exp a -> Exp a
  EBinop :: Op -> [Exp a] -> Exp a
  EIf :: Exp a -> Exp a -> Exp a -> Exp a
  EAssert :: Exp a -> Exp a -> Exp a
  ESeq :: Seq (Exp a) -> Exp a
  EUnit :: Exp a

deriving instance (Eq a) => Eq (Exp a)

deriving instance (Show a) => Show (Exp a)

var_of_exp :: (Show a) => Exp a -> Variable
var_of_exp e = case e of
  EVar x -> x
  _ -> failWith $ ErrMsg ("var_of_exp: expected variable: " ++ show e)

-- | Smart constructor for EBinop, ensuring all expressions (involving
--  associative operations) are flattened to top level.
exp_binop :: Op -> Exp a -> Exp a -> Exp a
exp_binop op e1 e2 =
  case (e1, e2) of
    (EBinop op1 l1, EBinop op2 l2)
      | op1 == op2 && op2 == op && isAssoc op ->
          EBinop op (l1 <> l2)
    (EBinop op1 l1, _)
      | op1 == op && isAssoc op ->
          EBinop op (l1 <> [e2])
    (_, EBinop op2 l2)
      | op2 == op && isAssoc op ->
          EBinop op (e1 : l2)
    (_, _) -> EBinop op [e1, e2]

-- | Smart constructor for sequence, ensuring all expressions are
--  flattened to top level.
exp_seq :: Exp a -> Exp a -> Exp a
exp_seq e1 e2 =
  case (e1, e2) of
    (ESeq l1, ESeq l2) -> ESeq (l1 >< l2)
    (ESeq l1, _) -> ESeq (l1 |> e2)
    (_, ESeq l2) -> ESeq (e1 <| l2)
    (_, _) -> ESeq (fromList [e1, e2])

is_pure :: Exp a -> Bool
is_pure e =
  case e of
    EVar _ -> True
    EVal _ -> True
    EUnop _ e1 -> is_pure e1
    EBinop _ es -> all is_pure es
    EIf b e1 e2 -> is_pure b && is_pure e1 && is_pure e2
    EAssert _ _ -> False
    ESeq es -> all is_pure es
    EUnit -> True

const_prop :: (GaloisField a) => Exp a -> State (Map Variable a) (Exp a)
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
    lookup_var :: (GaloisField a) => Variable -> State (Map Variable a) (Exp a)
    lookup_var x0 =
      gets
        ( \m -> case Map.lookup x0 m of
            Nothing -> EVar x0
            Just c -> EVal c
        )
    add_bind :: (Variable, a) -> State (Map Variable a) (Exp a)
    add_bind (x0, c0) =
      do
        modify (Map.insert x0 c0)
        return EUnit

do_const_prop :: (GaloisField a) => Exp a -> Exp a
do_const_prop e = evalState (const_prop e) Map.empty

instance (Pretty a) => Pretty (Exp a) where
  pretty (EVar x) = "exp_var_" <> pretty x
  pretty (EVal c) = pretty c
  pretty (EUnop op e1) = pretty op <> parens (pretty e1)
  pretty (EBinop op es) =
    hsep $ punctuate (" " <> pretty op <> " ") $ map pretty (toList es)
  pretty (EIf b e1 e2) =
    "if" <+> pretty b <+> "then" <+> pretty e1 <+> "else" <+> pretty e2
  pretty (EAssert e1 e2) = pretty e1 <+> ":=" <+> pretty e2
  pretty (ESeq es) = parens $ hsep $ punctuate ";" $ map pretty (toList es)
  pretty EUnit = "()"
