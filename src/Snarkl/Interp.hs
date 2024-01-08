{-# LANGUAGE LambdaCase #-}

module Snarkl.Interp
  ( interp,
  )
where

import Control.Monad (ap, foldM)
import Data.Data (Typeable)
import Data.Field.Galois (GaloisField)
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import Snarkl.Common (Op (..), UnOp (ZEq))
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Language (TExp, Variable, compileTExpToProgram)
import qualified Snarkl.Language.Core as Core

type Env a = Map Variable (Maybe a)

newtype InterpM a b = InterpM {runInterpM :: Env a -> Either ErrMsg (Env a, b)}

instance Monad (InterpM a) where
  (>>=) mf mg =
    InterpM
      ( \rho -> case runInterpM mf rho of
          Left err -> Left err
          Right (rho', b) -> runInterpM (mg b) rho'
      )
  return b =
    InterpM (\rho -> Right (rho, b))

instance Functor (InterpM a) where
  fmap f mg = return f `ap` mg

instance Applicative (InterpM a) where
  pure = return
  mf <*> ma = ap mf ma

raiseErr :: ErrMsg -> InterpM a b
raiseErr err =
  InterpM (\_ -> Left err)

addBinds :: [(Variable, Maybe a)] -> InterpM a (Maybe b)
addBinds binds =
  InterpM (\rho -> Right (Map.union (Map.fromList binds) rho, Nothing))

lookupVar :: (Show a) => Variable -> InterpM a (Maybe a)
lookupVar x =
  InterpM
    ( \rho -> case Map.lookup x rho of
        Nothing ->
          Left $
            ErrMsg $
              "unbound var "
                ++ show x
                ++ " in environment "
                ++ show rho
        Just v -> Right (rho, v)
    )

fieldOfBool :: (GaloisField a) => Bool -> a
fieldOfBool b = if b then 1 else 0

caseOfField :: (GaloisField a) => Maybe a -> (Maybe Bool -> InterpM a b) -> InterpM a b
caseOfField Nothing f = f Nothing
caseOfField (Just v) f
  | v == 0 = f $ Just False
  | v == 1 = f $ Just True
  | otherwise = raiseErr $ ErrMsg $ "expected " ++ show v ++ " to be boolean"

boolOfField :: (GaloisField a) => a -> InterpM a Bool
boolOfField v =
  caseOfField
    (Just v)
    ( \case
        Nothing -> raiseErr $ ErrMsg "internal error in bool_of_field"
        Just b -> return b
    )

interpTExp ::
  ( GaloisField a,
    Typeable ty
  ) =>
  TExp ty a ->
  InterpM a (Maybe a)
interpTExp e = do
  let _exp = compileTExpToProgram e
  interpProg _exp

interp ::
  (GaloisField a, Typeable ty) =>
  Map Variable a ->
  TExp ty a ->
  Either ErrMsg (Env a, Maybe a)
interp rho e = runInterpM (interpTExp e) $ Map.map Just rho

interpProg ::
  (GaloisField a) =>
  Core.Program a ->
  InterpM a (Maybe a)
interpProg (Core.Program assignments finalExp) =
  let f (Core.Assignment var e) = do
        e' <- interpCoreExpr e
        addBinds [(var, e')]
   in do
        traverse_ f assignments
        interpCoreExpr finalExp

interpCoreExpr ::
  (GaloisField a) =>
  Core.Exp a ->
  InterpM a (Maybe a)
interpCoreExpr = \case
  Core.EVar x -> lookupVar x
  Core.EVal v -> pure $ Just v
  Core.EUnop op e2 -> do
    v2 <- interpCoreExpr e2
    case v2 of
      Nothing -> pure Nothing
      Just v2' -> case op of
        ZEq -> return $ Just $ fieldOfBool (v2' == 0)
  Core.EBinop op _es -> case _es of
    [] -> failWith $ ErrMsg "empty binary args"
    (a : as) -> do
      b <- interpCoreExpr a
      foldM (interpBinopExpr op) b as
  Core.EIf eb e1 e2 ->
    do
      mb <- interpCoreExpr eb
      case mb of
        Nothing -> pure Nothing
        Just _b -> boolOfField _b >>= \b -> if b then interpCoreExpr e1 else interpCoreExpr e2
  Core.EUnit -> return $ Just 1
  where
    interpBinopExpr :: (GaloisField a) => Op -> Maybe a -> Core.Exp a -> InterpM a (Maybe a)
    interpBinopExpr _ Nothing _ = return Nothing
    interpBinopExpr _op (Just a1) _exp = do
      ma2 <- interpCoreExpr _exp
      case ma2 of
        Nothing -> return Nothing
        Just a2 -> Just <$> op a1 a2
      where
        op :: (GaloisField a) => a -> a -> InterpM a a
        op a b = case _op of
          Add -> pure $ a + b
          Sub -> pure $ a - b
          Mult -> pure $ a * b
          Div -> pure $ a / b
          And -> interpBooleanBinop a b
          Or -> interpBooleanBinop a b
          XOr -> interpBooleanBinop a b
          BEq -> interpBooleanBinop a b
          Eq -> pure $ fieldOfBool $ a == b
        interpBooleanBinop :: (GaloisField a) => a -> a -> InterpM a a
        interpBooleanBinop a b =
          do
            b1 <- boolOfField a
            b2 <- boolOfField b
            case _op of
              And -> return $ fieldOfBool $ b1 && b2
              Or -> return $ fieldOfBool $ b1 || b2
              XOr -> return $ fieldOfBool $ (b1 && not b2) || (b2 && not b1)
              BEq -> return $ fieldOfBool $ b1 == b2
              _ -> failWith $ ErrMsg "internal error in interp_binop"