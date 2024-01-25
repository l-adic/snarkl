{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use if" #-}

module Snarkl.AST.SyntaxMonad
  ( -- | Computation monad
    Comp,
    CompResult,
    runComp,
    return,
    (>>=),
    (>>),
    raise_err,
    Env (..),
    defaultEnv,
    InputVariable (..),
    -- | Return a fresh input variable.
    fresh_public_input,
    fresh_private_input,
    -- | Return a fresh variable.
    fresh_var,
    -- | Basic values
    unit,
    false,
    true,
    -- | Arrays
    arr,
    input_arr,
    get,
    set,
    -- | Pairs
    pair,
    fst_pair,
    snd_pair,
    -- | Basic static analysis
    is_true,
    is_false,
    assert_false,
    assert_true,
    is_bot,
    assert_bot,
    -- | Lambda
    lambda,
    -- | Misc. functions imported by 'Snarkl.Language.Prelude.hs'
    guard,
  )
where

import Control.Monad (forM, replicateM)
import Control.Monad.Supply (Supply, runSupply)
import Control.Monad.Supply.Class (MonadSupply (fresh))
import qualified Data.Map.Strict as Map
import Data.String (IsString (..))
import Data.Typeable (Typeable)
import Snarkl.AST.Expr (Variable (..))
import Snarkl.AST.TExpr
  ( Loc,
    TExp (TEAbs, TEAssert, TEBinop, TEBot, TESeq, TEUnop, TEVal, TEVar),
    TLoc (TLoc),
    TVar (TVar),
    Val (VFalse, VLoc, VTrue, VUnit),
    lastSeq,
    locOfTexp,
    teSeq,
    varOfTExp,
  )
import Snarkl.AST.Type (Ty (TArr, TBool, TFun, TProd, TUnit))
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Prelude hiding
  ( fromRational,
    negate,
    not,
    return,
    (&&),
    (*),
    (+),
    (-),
    (/),
    (>>),
    (>>=),
  )
import qualified Prelude as P

{-----------------------------------------------
 State Monad
------------------------------------------------}

type CompResult s a = Either ErrMsg (a, s)

newtype State s a = State (s -> CompResult s a)

runComp :: State s a -> s -> CompResult s a
runComp mf s = case mf of
  State f -> f s

raise_err :: ErrMsg -> Comp ty k
raise_err msg = State (\_ -> Left msg)

-- | We have to define our own bind operator, unfortunately,
-- because the "result" that's returned is the sequential composition
-- of the results of 'mf', 'g' (not just whatever 'g' returns)
(>>=) ::
  forall (ty1 :: Ty) (ty2 :: Ty) s a.
  State s (TExp ty1 a) ->
  (TExp ty1 a -> State s (TExp ty2 a)) ->
  State s (TExp ty2 a)
(>>=) mf g =
  State
    ( \s ->
        case runComp mf s of
          Left err -> Left err
          Right (e, s') -> case runComp (g e) s' of
            Left err -> Left err
            Right (e', s'') -> Right (e `teSeq` e', s'')
    )

(>>) ::
  forall (ty1 :: Ty) (ty2 :: Ty) s a.
  State s (TExp ty1 a) ->
  State s (TExp ty2 a) ->
  State s (TExp ty2 a)
(>>) mf g = do _ <- mf; g

return :: TExp ty a -> State s (TExp ty a)
return e = State (\s -> Right (lastSeq e, s))

-- | At elaboration time, we maintain an environment containing
--    (i) next_var:  the next free variable
--    (ii) next_loc: the next fresh location
--    (iii) obj_map: a symbol table mapping (obj_loc,integer index) to
--    the constraint variable associated with that object, at that
--    field index. A given (obj_loc,integer index) pair may also
--    resolve to a constant rational, boolean, or the bottom value,
--    for constant propagation.
--
--  Reading from object 'a' at index 'i' (x := a_i) corresponds to:
--    (a) getting y <- obj_map(a,i)
--    (b) inserting the constraint (x = y), if x,y resolve to logic
--    vars.
data ObjBind
  = ObjLoc Loc
  | ObjVar Variable
  deriving
    ( Show
    )

data AnalBind k
  = AnalBool Bool
  | AnalConst k
  | AnalBot
  deriving
    ( Show
    )

type ObjMap =
  Map.Map
    ( Loc, -- object a
      Int -- at index i
    )
    ObjBind -- maps to result r

data InputVariable
  = PublicInput Variable
  | PrivateInput String Variable -- (unique) name, variable
  deriving (Show, Eq, Ord)

data Env k = Env
  { next_variable :: Int,
    next_loc :: Int,
    input_vars :: [InputVariable],
    obj_map :: ObjMap,
    anal_map :: Map.Map Variable (AnalBind k) -- supporting simple constprop analyses
  }
  deriving (Show)

defaultEnv :: Env k
defaultEnv =
  Env
    { next_variable = 0,
      next_loc = 0,
      input_vars = [],
      obj_map = Map.empty,
      anal_map = Map.empty
    }

type Comp ty k = State (Env k) (TExp ty k)

{-----------------------------------------------
 Units, Booleans (used below)
------------------------------------------------}

unit :: TExp 'TUnit k
unit = TEVal VUnit

false :: TExp 'TBool k
false = TEVal VFalse

true :: TExp 'TBool k
true = TEVal VTrue

{-----------------------------------------------
 Arrays
------------------------------------------------}

arr :: Int -> Comp ('TArr ty) k
arr 0 = raise_err $ ErrMsg "array must have size > 0"
arr len =
  State
    ( \s ->
        let loc = next_loc s
            (binds, nextVar) = runSupply (new_binds loc) (next_variable s)
         in Right
              ( TEVal (VLoc (TLoc $ next_loc s)),
                -- allocate:
                -- (1) a new location (next_loc s)
                -- (2) 'len' new variables [(next_variable s)..(next_variable s+len-1)]
                s
                  { next_variable = nextVar,
                    next_loc = loc P.+ 1,
                    obj_map = binds `Map.union` obj_map s
                  }
              )
    )
  where
    new_binds :: Loc -> Supply ObjMap
    new_binds loc =
      Map.fromList
        <$> forM
          [0 .. (len P.- 1)]
          ( \i ->
              fresh P.>>= \v ->
                pure
                  ( (loc, i),
                    ObjVar (Variable v)
                  )
          )

-- Like 'arr', but declare fresh array variables as inputs.
input_arr :: Int -> Comp ('TArr ty) k
input_arr 0 = raise_err $ ErrMsg "array must have size > 0"
input_arr len =
  State
    ( \s ->
        let loc = next_loc s
            ((binds, vars), nextVar) = runSupply (new_binds loc) (next_variable s)
         in Right
              ( TEVal (VLoc (TLoc $ next_loc s)),
                -- allocate:
                -- (1) a new location (next_loc s)
                -- (2) 'len' new variables [(next_variable s)..(next_variable s+len-1)]
                -- (3) mark new vars. as inputs
                s
                  { next_variable = nextVar,
                    next_loc = loc P.+ 1,
                    input_vars = map PublicInput vars <> input_vars s,
                    obj_map = binds `Map.union` obj_map s
                  }
              )
    )
  where
    new_binds :: Loc -> Supply (ObjMap, [Variable])
    new_binds loc =
      new_vars P.>>= \vs ->
        pure
          ( Map.fromList $ zipWith (\i v -> ((loc, i), ObjVar v)) [0 .. (len P.- 1)] vs,
            vs
          )
    new_vars :: Supply [Variable]
    new_vars = replicateM len (Variable <$> fresh)

get_addr :: (Loc, Int) -> Comp ty k
get_addr (l, i) =
  State
    ( \s -> case Map.lookup (l, i) (obj_map s) of
        Just (ObjLoc l') -> Right (TEVal (VLoc (TLoc l')), s)
        Just (ObjVar x) -> Right (TEVar (TVar x), s)
        Nothing ->
          Left $
            ErrMsg
              ( "unbound loc "
                  ++ show (l, i)
                  ++ " in heap "
                  ++ show (obj_map s)
              )
    )

guard ::
  (Typeable ty2) =>
  (TExp ty k -> Comp ty2 k) ->
  TExp ty k ->
  Comp ty2 k
guard f e =
  do
    b <- is_bot e
    case b of
      TEVal VTrue -> return TEBot
      TEVal VFalse -> f e
      _ -> failWith $ ErrMsg "internal error in guard"

guarded_get_addr ::
  (Typeable ty2) =>
  TExp ty k ->
  Int ->
  Comp ty2 k
guarded_get_addr e i =
  guard (\e0 -> get_addr (locOfTexp e0, i)) e

get :: (Typeable ty) => (TExp ('TArr ty) k, Int) -> Comp ty k
get (TEBot, _) = return TEBot
get (a, i) = guarded_get_addr a i

-- | Smart constructor for TEAssert
te_assert :: (Typeable ty) => TExp ty k -> TExp ty k -> Comp 'TUnit k
te_assert x@(TEVar _) e =
  do
    e_bot <- is_bot e
    e_true <- is_true e
    e_false <- is_false e
    case (e_bot, e_true, e_false) of
      (TEVal VTrue, _, _) -> assert_bot x >> return (TEAssert x e)
      (_, TEVal VTrue, _) -> assert_true x >> return (TEAssert x e)
      (_, _, TEVal VTrue) -> assert_false x >> return (TEAssert x e)
      _ -> return $ TEAssert x e
te_assert _ e =
  failWith $
    ErrMsg $
      "in te_assert, expected var but got " ++ show e

-- | Update array 'a' at position 'i' to expression 'e'. We special-case
-- variable and location expressions, because they're representable untyped
-- in the object map.
set_addr ::
  (Typeable ty) =>
  (TExp ('TArr ty) k, Int) ->
  TExp ty k ->
  Comp 'TUnit k
-- The following specialization (to variable expressions) is an
-- optimization: we avoid introducing a fresh variable.
set_addr (TEVal (VLoc (TLoc l)), i) (TEVar (TVar x)) =
  add_objects [((l, i), ObjVar x)] >> return unit
-- The following specialization (to location values) is necessary to
-- satisfy [INVARIANT]: All expressions of compound types (sums,
-- products, arrays, ...) have the form (TEVal (VLoc (TLoc l))), for
-- some location l.
set_addr (TEVal (VLoc (TLoc l)), i) (TEVal (VLoc (TLoc l'))) =
  do
    _ <- add_objects [((l, i), ObjLoc l')]
    return unit

-- Default:
set_addr (TEVal (VLoc (TLoc l)), i) e =
  do
    x <- fresh_var
    _ <- add_objects [((l, i), ObjVar (varOfTExp x))]
    te_assert x e

-- Err: expression does not satisfy [INVARIANT].
set_addr (e1, _) _ =
  raise_err $ ErrMsg ("expected " ++ show e1 ++ " a loc")

set :: (Typeable ty) => (TExp ('TArr ty) k, Int) -> TExp ty k -> Comp 'TUnit k
set (a, i) e = set_addr (a, i) e

{-----------------------------------------------
 Products
------------------------------------------------}

pair ::
  ( Typeable ty1,
    Typeable ty2
  ) =>
  TExp ty1 k ->
  TExp ty2 k ->
  Comp ('TProd ty1 ty2) k
pair te1 te2 =
  do
    l <- fresh_loc
    _ <- add_binds (locOfTexp l) (lastSeq te1) (lastSeq te2)
    return l
  where
    add_binds l (TEVal (VLoc (TLoc l1))) (TEVal (VLoc (TLoc l2))) =
      add_objects [((l, 0), ObjLoc l1), ((l, 1), ObjLoc l2)]
    add_binds l (TEVal (VLoc (TLoc l1))) e2 =
      do
        x2 <- fresh_var
        _ <- add_objects [((l, 0), ObjLoc l1), ((l, 1), ObjVar $ varOfTExp x2)]
        te_assert x2 e2
    add_binds l e1 (TEVal (VLoc (TLoc l2))) =
      do
        x1 <- fresh_var
        _ <- add_objects [((l, 0), ObjVar $ varOfTExp x1), ((l, 1), ObjLoc l2)]
        te_assert x1 e1
    add_binds l e1 e2 =
      do
        x1 <- fresh_var
        x2 <- fresh_var
        _ <-
          add_objects
            [ ((l, 0), ObjVar $ varOfTExp x1),
              ((l, 1), ObjVar $ varOfTExp x2)
            ]
        -- NOTE: return e ~~> return (lastSeq e). So we rely on the
        -- slightly weird semantics of (>>=) to do the sequencing of
        -- the two assertions for us.
        _ <- te_assert x1 e1
        te_assert x2 e2

fst_pair ::
  (Typeable ty1) =>
  TExp ('TProd ty1 ty2) k ->
  Comp ty1 k
fst_pair TEBot = return TEBot
fst_pair e = guarded_get_addr e 0

snd_pair ::
  ( Typeable ty2
  ) =>
  TExp ('TProd ty1 ty2) k ->
  Comp ty2 k
snd_pair TEBot = return TEBot
snd_pair e = guarded_get_addr e 1

{-----------------------------------------------
 Auxiliary functions
------------------------------------------------}

fresh_var :: Comp ty a
fresh_var =
  State
    ( \s ->
        let (v, nextVar) = runSupply (Variable <$> fresh) (next_variable s)
         in Right
              ( TEVar (TVar v),
                s
                  { next_variable = nextVar
                  }
              )
    )

fresh_public_input :: Comp ty a
fresh_public_input =
  State
    ( \s ->
        let (v, nextVar) = runSupply (Variable <$> fresh) (next_variable s)
         in Right
              ( TEVar (TVar v),
                s
                  { next_variable = nextVar,
                    input_vars = PublicInput v : input_vars s
                  }
              )
    )

fresh_private_input :: String -> Comp ty a
fresh_private_input name = do
  v <- fresh_var
  State
    ( \s ->
        Right
          ( v,
            s
              { input_vars =
                  -- rebindable syntax is on, so no ifThenElse syntax allowed
                  case variableExists s of
                    True -> error $ "variable already exists: " <> name
                    False -> PrivateInput name (varOfTExp v) : input_vars s
              }
          )
    )
  where
    variableExists =
      any
        ( \case
            PrivateInput name' _ -> name == name'
            _ -> False
        )
        . input_vars

fresh_loc :: Comp ty a
fresh_loc =
  State
    ( \s ->
        Right
          ( TEVal (VLoc (TLoc $ next_loc s)),
            s
              { next_loc = (P.+) (next_loc s) 1
              }
          )
    )

add_objects :: [((Loc, Int), ObjBind)] -> Comp 'TUnit k
add_objects binds =
  State
    ( \s ->
        Right
          ( unit,
            s
              { obj_map = Map.fromList binds `Map.union` obj_map s
              }
          )
    )

add_statics :: [(Variable, AnalBind k)] -> Comp 'TUnit k
add_statics binds =
  State
    ( \s ->
        Right
          ( unit,
            s
              { anal_map = Map.fromList binds `Map.union` anal_map s
              }
          )
    )

-- | Does boolean expression 'e' resolve (statically) to 'b'?
is_bool :: TExp ty k -> Bool -> Comp 'TBool k
is_bool (TEVal VFalse) False = return true
is_bool (TEVal VTrue) True = return true
is_bool e@(TEVar _) b =
  State
    ( \s ->
        Right
          ( case Map.lookup (varOfTExp e) (anal_map s) of
              Nothing -> false
              Just (AnalBool b') | b /= b' -> false
              Just (AnalBool b') | b == b' -> true
              Just _ | otherwise -> false,
            s
          )
    )
is_bool _ _ = return false

is_false :: TExp ty k -> Comp 'TBool k
is_false = flip is_bool False

is_true :: TExp ty k -> Comp 'TBool k
is_true = flip is_bool True

-- | Add binding 'x = b'.
assert_bool :: TExp ty k -> Bool -> Comp 'TUnit k
assert_bool (TEVar (TVar x)) b = add_statics [(x, AnalBool b)]
assert_bool e _ = raise_err $ ErrMsg $ "expected " ++ show e ++ " a variable"

assert_false :: TExp ty k -> Comp 'TUnit k
assert_false = flip assert_bool False

assert_true :: TExp ty k -> Comp 'TUnit k
assert_true = flip assert_bool True

var_is_bot :: TExp ty k -> Comp 'TBool k
var_is_bot e@(TEVar (TVar _)) =
  State
    ( \s ->
        Right
          ( case Map.lookup (varOfTExp e) (anal_map s) of
              Nothing -> false
              Just AnalBot -> true
              Just _ -> false,
            s
          )
    )
var_is_bot _ = return false

is_bot :: TExp ty k -> Comp 'TBool k
is_bot e =
  case e of
    e0@(TEVar _) -> var_is_bot e0
    TEUnop _ e0 -> is_bot e0
    TEBinop _ e1 e2 -> either_is_bot e1 e2
    TESeq e1 e2 -> either_is_bot e1 e2
    TEBot -> return true
    _ -> return false
  where
    either_is_bot :: TExp ty1 k -> TExp ty2 k -> Comp 'TBool k
    either_is_bot e10 e20 =
      do
        e1_bot <- is_bot e10
        e2_bot <- is_bot e20
        case (e1_bot, e2_bot) of
          (TEVal VTrue, _) -> return true
          (_, TEVal VTrue) -> return true
          _ -> return false

assert_bot :: TExp ty k -> Comp 'TUnit k
assert_bot (TEVar (TVar x)) = add_statics [(x, AnalBot)]
assert_bot e = raise_err $ ErrMsg $ "in assert_bot, expected " ++ show e ++ " a variable"

lambda ::
  (Typeable a) =>
  (Typeable b) =>
  (TExp a k -> Comp b k) ->
  Comp ('TFun a b) k
lambda f = do
  _x <- fresh_var
  case _x of
    TEVar x ->
      -- we need to inline the monadic computation to avoid having
      -- bound variable escape there scope in assertions for (f _x)
      State
        ( \s ->
            case runComp (f _x) s of
              Left err -> Left err
              Right (res, s') -> Right (TEAbs x res, s')
        )
    _ -> error "impossible: lambda"
