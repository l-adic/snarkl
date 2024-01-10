{-# LANGUAGE RebindableSyntax #-}

module Snarkl.Syntax
  ( -- | Snarkl.Language.TExpr,
    coerceToInt,
    TExp,
    Ty (..),
    TFunct (..),
    Rep,
    -- | SyntaxMonad and Syntax
    Comp,
    runComp,
    return,
    ifThenElse_aux,
    (>>=),
    (>>),
    -- | Return a fresh input variable.
    fresh_input,
    -- | Classes
    Zippable,
    Derive,
    -- | Basic values
    unit,
    false,
    true,
    fromField,
    -- | Sums, products, recursive types
    inl,
    inr,
    case_sum,
    pair,
    fst_pair,
    snd_pair,
    roll,
    unroll,
    fixN,
    fix,
    -- | Arithmetic and boolean operations
    (+),
    (-),
    (*),
    (/),
    (&&),
    (||),
    zeq,
    not,
    xor,
    eq,
    beq,
    exp_of_int,
    inc,
    dec,
    ifThenElse,
    negate,
    -- | Arrays
    arr,
    arr2,
    arr3,
    input_arr,
    input_arr2,
    input_arr3,
    set,
    set2,
    set3,
    set4,
    get,
    get2,
    get3,
    get4,
    -- | Iteration
    iter,
    iterM,
    bigsum,
    times,
    forall,
    forall2,
    forall3,
    -- | Function combinators
    lambda,
    curry,
    uncurry,
    apply,
    assert_bot,
    assert_false,
    assert_true,
    defaultEnv,
    fresh_var,
    guard,
    is_bot,
    is_false,
    is_true,
    raise_err,
    runState,
    -- | Vectors
    vec,
    getV,
    getV2,
    setV,
    setV2,
    mapV,
    foldlV,
    traverseV,
    traverseV_,
    concatV,
    chunkV,
    allV,
    transpose,
    unsafe_cast,
  )
where

import Data.Field.Galois (GaloisField, PrimeField, fromP)
import Data.Fin (Fin, toNatural, universe)
import Data.Nat (Nat (..))
import Data.String (IsString (..))
import Data.Type.Nat (Mult, SNatI, reflectToNum)
import Data.Typeable (Proxy (Proxy), Typeable)
import Debug.Trace (trace)
import Snarkl.Common
  ( Op (..),
    UnOp (..),
  )
import Snarkl.Errors (ErrMsg (ErrMsg))
import Snarkl.Language.SyntaxMonad
  ( Comp,
    Env,
    State (..),
    arr,
    assert_bot,
    assert_false,
    assert_true,
    defaultEnv,
    false,
    fresh_input,
    fresh_var,
    fst_pair,
    get,
    guard,
    input_arr,
    is_bot,
    is_false,
    is_true,
    pair,
    raise_err,
    return,
    runComp,
    runState,
    set,
    snd_pair,
    true,
    unit,
    (>>),
    (>>=),
  )
import Snarkl.Language.TExpr
  ( TExp (TEAbs, TEApp, TEBinop, TEBot, TEIf, TEUnop, TEVal, TEVar),
    TOp (TOp),
    TUnop (TUnop),
    Val (VFalse, VField, VTrue, VUnit),
  )
import Snarkl.Language.Type (Rep, TFunct (..), Ty (..))
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding
  ( all,
    concat,
    curry,
    foldl,
    fromRational,
    negate,
    not,
    return,
    traverse,
    uncurry,
    (&&),
    (*),
    (+),
    (-),
    (/),
    (<*>),
    (>>),
    (>>=),
    (||),
  )
import qualified Prelude as P

inc :: Int -> Int
inc n = (P.+) n 1

dec :: Int -> Int
dec n = (P.-) n 1

----------------------------------------------------
--
-- Arrays
--
----------------------------------------------------

-- | 2-d arrays. 'width' is the size, in "bits" (#field elements), of
-- each array element.
arr2 :: (Typeable ty) => Int -> Int -> Comp ('TArr ('TArr ty)) k
arr2 len width =
  do
    a <- arr len
    _ <-
      forall
        [0 .. dec len]
        ( \i ->
            do
              ai <- arr width
              set (a, i) ai
        )
    return a

-- | 3-d arrays.
arr3 :: (Typeable ty) => Int -> Int -> Int -> Comp ('TArr ('TArr ('TArr ty))) k
arr3 len width height =
  do
    a <- arr2 len width
    _ <-
      forall2
        ([0 .. dec len], [0 .. dec width])
        ( \i j ->
            do
              aij <- arr height
              set2 (a, i, j) aij
        )
    return a

input_arr2 :: (Typeable ty) => Int -> Int -> Comp ('TArr ('TArr ty)) k
input_arr2 0 _ = raise_err $ ErrMsg "array must have size > 0"
input_arr2 len width =
  do
    a <- arr len
    _ <-
      forall
        [0 .. dec len]
        ( \i ->
            do
              ai <- input_arr width
              set (a, i) ai
        )
    return a

input_arr3 :: (Typeable ty) => Int -> Int -> Int -> Comp ('TArr ('TArr ('TArr ty))) k
input_arr3 len width height =
  do
    a <- arr2 len width
    _ <-
      forall2
        ([0 .. dec len], [0 .. dec width])
        ( \i j ->
            do
              aij <- input_arr height
              set2 (a, i, j) aij
        )
    return a

set2 :: (Typeable ty2) => (TExp ('TArr ('TArr ty2)) k, Int, Int) -> TExp ty2 k -> Comp 'TUnit k
set2 (a, i, j) e = do
  a' <- get (a, i)
  set (a', j) e

set3 ::
  (Typeable ty) =>
  ( TExp ('TArr ('TArr ('TArr ty))) k,
    Int,
    Int,
    Int
  ) ->
  TExp ty k ->
  State (Env k) (TExp 'TUnit k)
set3 (a, i, j, k) e = do
  a' <- get2 (a, i, j)
  set (a', k) e

set4 ::
  (Typeable ty) =>
  ( TExp ('TArr ('TArr ('TArr ('TArr ty)))) k,
    Int,
    Int,
    Int,
    Int
  ) ->
  TExp ty k ->
  State (Env k) (TExp 'TUnit k)
set4 (a, i, j, k, l) e = do
  a' <- get3 (a, i, j, k)
  set (a', l) e

get2 :: (Typeable ty2) => (TExp ('TArr ('TArr ty2)) k, Int, Int) -> State (Env k) (TExp ty2 k)
get2 (a, i, j) = do
  a' <- get (a, i)
  get (a', j)

get3 ::
  (Typeable ty) =>
  ( TExp ('TArr ('TArr ('TArr ty))) k,
    Int,
    Int,
    Int
  ) ->
  State (Env k) (TExp ty k)
get3 (a, i, j, k) = do
  a' <- get2 (a, i, j)
  get (a', k)

get4 ::
  (Typeable ty) =>
  ( TExp ('TArr ('TArr ('TArr ('TArr ty)))) k,
    Int,
    Int,
    Int,
    Int
  ) ->
  State (Env k) (TExp ty k)
get4 (a, i, j, k, l) = do
  a' <- get3 (a, i, j, k)
  get (a', l)

----------------------------------------------------
--
-- Sums
--
----------------------------------------------------

rep_sum ::
  TExp ('TSum ty1 ty2) k ->
  TExp ('TProd 'TBool ('TProd ty1 ty2)) k
rep_sum = unsafe_cast

unrep_sum ::
  TExp ('TProd 'TBool ('TProd ty1 ty2)) k ->
  TExp ('TSum ty1 ty2) k
unrep_sum = unsafe_cast

inl ::
  forall ty1 ty2 k.
  ( Typeable ty1,
    Typeable ty2
  ) =>
  TExp ty1 k ->
  Comp ('TSum ty1 ty2) k
inl te1 =
  do
    let v2 = TEBot
    y <- pair te1 v2
    v2_var <- snd_pair y
    _ <- assert_bot v2_var
    z <- pair (TEVal VFalse) y
    z_fst <- fst_pair z
    _ <- assert_false z_fst
    return $ unrep_sum z

inr ::
  forall ty1 ty2 k.
  ( Typeable ty1,
    Typeable ty2
  ) =>
  TExp ty2 k ->
  Comp ('TSum ty1 ty2) k
inr te2 =
  do
    let v1 = TEBot
    y <- pair v1 te2
    v1_var <- fst_pair y
    _ <- assert_bot v1_var
    z <- pair (TEVal VTrue) y
    z_fst <- fst_pair z
    _ <- assert_true z_fst
    return $ unrep_sum z

case_sum ::
  forall ty1 ty2 ty k.
  ( Typeable ty1,
    Typeable ty2,
    Zippable ty k
  ) =>
  (TExp ty1 k -> Comp ty k) ->
  (TExp ty2 k -> Comp ty k) ->
  TExp ('TSum ty1 ty2) k ->
  Comp ty k
case_sum f1 f2 e =
  do
    let p = rep_sum e
    b <- fst_pair p
    is_inl <- is_false b
    is_inr <- is_true b
    p_rest <- snd_pair p
    e1 <- fst_pair p_rest
    e2 <- snd_pair p_rest
    case is_inl of
      TEVal VTrue -> f1 e1
      _ -> case is_inr of
        TEVal VTrue -> f2 e2
        _ -> do
          le <- f1 e1
          re <- f2 e2
          -- NOTE: should not guard b here.
          -- zip_vals ... must maintain Snarkl.Language.SyntaxMonad [INVARIANT]
          -- regarding the representation of nonbase-type expressions.
          zip_vals (not b) le re

-- | Types for which a default value is derivable
class Derive ty k where
  derive :: Int -> Comp ty k

instance Derive 'TUnit k where
  derive _ = return $ TEVal VUnit

instance Derive 'TBool k where
  derive _ = return $ TEVal VFalse

instance (GaloisField k) => Derive 'TField k where
  derive _ = return $ TEVal (VField 0)

instance (Typeable ty, Derive ty k) => Derive ('TArr ty) k where
  derive n =
    do
      a <- arr 1
      v <- derive n
      _ <- set (a, 0) v
      return a

instance
  ( Typeable ty1,
    Derive ty1 k,
    Typeable ty2,
    Derive ty2 k
  ) =>
  Derive ('TProd ty1 ty2) k
  where
  derive n =
    do
      v1 <- derive n
      v2 <- derive n
      pair v1 v2

instance
  ( Typeable ty1,
    Derive ty1 k,
    Typeable ty2
  ) =>
  Derive ('TSum ty1 ty2) k
  where
  derive n =
    do
      v1 <- derive n
      inl v1

instance
  ( Derive (Rep f ('TMu f)) k
  ) =>
  Derive ('TMu f) k
  where
  derive n
    | n > 0 =
        do
          v1 <- derive (dec n)
          roll v1
    | otherwise =
        do
          x <- fresh_var
          _ <- assert_bot x
          return x

instance (Typeable a, Typeable b, Derive b k) => Derive ('TFun a b) k where
  derive n = lambda $ \_ -> derive n

-- | Types for which conditional branches can be pushed to the leaves
-- of two values.
class Zippable ty k where
  zip_vals ::
    TExp 'TBool k ->
    TExp ty k ->
    TExp ty k ->
    Comp ty k

instance Zippable 'TUnit k where
  zip_vals _ _ _ = return unit

zip_base :: (Typeable ty) => TExp 'TBool k -> TExp ty k -> TExp ty k -> Comp ty k
zip_base TEBot _ _ = return TEBot
zip_base _ TEBot e2 = return e2
zip_base _ e1 TEBot = return e1
zip_base b e1 e2 =
  do
    b_true <- is_true b
    b_false <- is_false b
    case (b_true, b_false) of
      (TEVal VTrue, _) -> return e1
      (_, TEVal VTrue) -> return e2
      _ ->
        guard
          ( \b0 ->
              do
                e1_bot <- is_bot e1
                e2_bot <- is_bot e2
                case (e1_bot, e2_bot) of
                  (TEVal VTrue, _) -> return e2
                  (_, TEVal VTrue) -> return e1
                  _ -> return $ ifThenElse_aux b0 e1 e2
          )
          b

instance Zippable 'TBool k where
  zip_vals b b1 b2 = zip_base b b1 b2

instance Zippable 'TField k where
  zip_vals b e1 e2 = zip_base b e1 e2

fuel :: Int
fuel = 1

check_bots ::
  ( Derive ty k
  ) =>
  Comp ty k ->
  TExp 'TBool k ->
  TExp ty k ->
  TExp ty k ->
  Comp ty k
check_bots f b e1 e2 =
  do
    b_true <- is_true b
    b_false <- is_false b
    b_bot <- is_bot b
    e1_bot <- is_bot e1
    e2_bot <- is_bot e2
    case (b_true, b_false, b_bot, e1_bot, e2_bot) of
      (TEVal VTrue, _, _, _, _) -> return e1
      (_, TEVal VTrue, _, _, _) -> return e2
      (_, _, TEVal VTrue, _, _) -> derive fuel
      (_, _, _, TEVal VTrue, TEVal VTrue) -> derive fuel
      (_, _, _, TEVal VTrue, TEVal VFalse) -> return e2
      (_, _, _, TEVal VFalse, TEVal VTrue) -> return e1
      (_, _, _, TEVal VFalse, TEVal VFalse) -> f
      (_, _, _, _, _) -> raise_err $ ErrMsg "internal error in check_bots"

instance
  ( Zippable ty1 k,
    Typeable ty1,
    Derive ty1 k,
    Zippable ty2 k,
    Typeable ty2,
    Derive ty2 k
  ) =>
  Zippable ('TProd ty1 ty2) k
  where
  zip_vals b e1 e2 = check_bots f b e1 e2
    where
      f = do
        e11 <- fst_pair e1
        e12 <- snd_pair e1
        e21 <- fst_pair e2
        e22 <- snd_pair e2
        p1 <- zip_vals b e11 e21
        p2 <- zip_vals b e12 e22
        pair p1 p2

instance
  ( Zippable ty1 k,
    Typeable ty1,
    Derive ty1 k,
    Zippable ty2 k,
    Typeable ty2,
    Derive ty2 k
  ) =>
  Zippable ('TSum ty1 ty2) k
  where
  zip_vals b e1 e2 = check_bots f b e1 e2
    where
      f = do
        let p1 = rep_sum e1
        let p2 = rep_sum e2
        p' <- zip_vals b p1 p2
        return $ unrep_sum p'

instance
  ( Zippable (Rep f ('TMu f)) k,
    Derive (Rep f ('TMu f)) k
  ) =>
  Zippable ('TMu f) k
  where
  zip_vals b e1 e2 = check_bots f b e1 e2
    where
      f = do
        e1' <- unroll e1
        e2' <- unroll e2
        x <- zip_vals b e1' e2'
        roll x

instance Zippable ('TArr ty) k where
  zip_vals _ x _ = return x

instance
  ( Zippable ty1 k,
    Typeable ty1,
    Derive ty1 k,
    Zippable ty2 k,
    Typeable ty2,
    Derive ty2 k
  ) =>
  Zippable ('TFun ty1 ty2) k
  where
  zip_vals b e1 e2 = do
    y1 <- lambda $ \x ->
      return $ TEApp e1 x
    y2 <- lambda $ \x ->
      return $ TEApp e2 x
    zip_vals b y1 y2

----------------------------------------------------
--
-- Recursive Types
--
----------------------------------------------------

unsafe_cast :: TExp ty1 k -> TExp ty2 k
unsafe_cast = unsafeCoerce

unroll ::
  TExp ('TMu f) k ->
  Comp (Rep f ('TMu f)) k
unroll te = return $ unsafe_cast te

roll ::
  TExp (Rep f ('TMu f)) k ->
  Comp ('TMu f) k
roll te = return $ unsafe_cast te

fixN ::
  (Typeable ty2) =>
  Int ->
  ( (TExp ty1 k -> Comp ty2 k) ->
    TExp ty1 k ->
    Comp ty2 k
  ) ->
  TExp ty1 k ->
  Comp ty2 k
fixN depth f e = go depth e
  where
    -- WARNING: We only handle inductive data up to size 'depth'.
    go 0 _ = return TEBot
    go n e0 = f (go (dec n)) e0

fix ::
  (Typeable ty2) =>
  ( (TExp ty1 k -> Comp ty2 k) ->
    TExp ty1 k ->
    Comp ty2 k
  ) ->
  TExp ty1 k ->
  Comp ty2 k
fix = fixN 100

{-
  fix f = let x = f x in x

fix' :: TExp ('TFun a a) k -> Comp a k
fix' f =
  let x = f `apply` x
  in x

fix' :: TExp ('TFun a a) k -> Comp a k
fix' f = do
  x <- f `apply` x
  return x

-}

----------------------------------------------------
--
-- Operators, Values
--
----------------------------------------------------

(+) :: TExp 'TField k -> TExp 'TField k -> TExp 'TField k
(+) e1 e2 = TEBinop (TOp Add) e1 e2

(-) :: TExp 'TField k -> TExp 'TField k -> TExp 'TField k
(-) e1 e2 = TEBinop (TOp Sub) e1 e2

(*) :: TExp 'TField k -> TExp 'TField k -> TExp 'TField k
(*) e1 e2 = TEBinop (TOp Mult) e1 e2

(/) :: TExp 'TField k -> TExp 'TField k -> TExp 'TField k
(/) e1 e2 = TEBinop (TOp Div) e1 e2

(||) :: TExp 'TBool k -> TExp 'TBool k -> TExp 'TBool k
(||) e1 e2 = TEBinop (TOp Or) e1 e2

(&&) :: TExp 'TBool k -> TExp 'TBool k -> TExp 'TBool k
(&&) e1 e2 = TEBinop (TOp And) e1 e2

zeq :: TExp 'TField k -> TExp 'TBool k
zeq e = TEUnop (TUnop ZEq) e

not :: TExp 'TBool k -> TExp 'TBool k
not e = ifThenElse_aux e false true

xor :: TExp 'TBool k -> TExp 'TBool k -> TExp 'TBool k
xor e1 e2 = TEBinop (TOp XOr) e1 e2

beq :: TExp 'TBool k -> TExp 'TBool k -> TExp 'TBool k
beq e1 e2 = TEBinop (TOp BEq) e1 e2

eq :: (Typeable ty) => TExp ty k -> TExp ty k -> TExp 'TBool k
eq e1 e2 = TEBinop (TOp Eq) e1 e2

fromField :: (GaloisField k) => k -> TExp 'TField k
fromField r = TEVal (VField r)

exp_of_int :: (GaloisField k) => Int -> TExp 'TField k
exp_of_int i = TEVal (VField $ fromIntegral i)

ifThenElse_aux ::
  TExp 'TBool a ->
  TExp ty a ->
  TExp ty a ->
  TExp ty a
ifThenElse_aux b e1 e2
  | e1 == e2 = e1
  | otherwise =
      case b of
        TEVal VFalse -> e2
        TEVal VTrue -> e1
        _ -> TEIf b e1 e2

ifThenElse ::
  ( Zippable ty k
  ) =>
  Comp 'TBool k ->
  Comp ty k ->
  Comp ty k ->
  Comp ty k
ifThenElse cb c1 c2 =
  do
    b <- cb
    e1 <- c1
    e2 <- c2
    zip_vals b e1 e2

negate :: (GaloisField k) => TExp 'TField k -> TExp 'TField k
negate e = fromField (P.negate 1) * e

----------------------------------------------------
--
-- Iteration
--
----------------------------------------------------

iter ::
  Int ->
  (Int -> TExp ty k -> TExp ty k) ->
  TExp ty k ->
  TExp ty k
iter n f e = g n f e
  where
    g 0 f' e' = f' 0 e'
    g m f' e' = f' m $ g (dec m) f' e'

iterM ::
  Int ->
  (Int -> TExp ty k -> Comp ty k) ->
  TExp ty k ->
  Comp ty k
iterM n mf e = g n mf e
  where
    g 0 mf' e' = mf' 0 e'
    g m mf' e' =
      do
        x <- g (dec m) mf' e'
        mf' m x

bigsum ::
  (GaloisField k) =>
  Int ->
  (Int -> TExp 'TField k) ->
  TExp 'TField k
bigsum n f = iter n (\n' e -> f n' + e) (fromField 0)

forall ::
  [a] ->
  (a -> Comp 'TUnit k) ->
  Comp 'TUnit k
forall as mf = g as mf
  where
    g [] _ = return unit
    g (a : as') mf' =
      do _ <- mf' a; g as' mf'

forall2 :: ([a], [b]) -> (a -> b -> Comp 'TUnit k) -> Comp 'TUnit k
forall2 (as1, as2) mf =
  forall as1 (forall as2 . mf)

forall3 :: ([a], [b], [c]) -> (a -> b -> c -> Comp 'TUnit k) -> Comp 'TUnit k
forall3 (as1, as2, as3) mf =
  forall2 (as1, as2) (\a1 a2 -> forall as3 (mf a1 a2))

times ::
  Int ->
  Comp 'TUnit k ->
  Comp 'TUnit k
times n mf = forall [0 .. dec n] (const mf)

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
            case runState (f _x) s of
              Left err -> Left err
              Right (res, s') -> Right (TEAbs x res, s')
        )
    _ -> error "impossible: lambda"

curry ::
  (Typeable a) =>
  (Typeable b) =>
  (Typeable c) =>
  (TExp ('TProd a b) k -> Comp c k) ->
  TExp a k ->
  Comp ('TFun b c) k
curry f a = do
  lambda $ \b -> do
    p <- pair a b
    f p

uncurry ::
  (Typeable a) =>
  (Typeable b) =>
  (Typeable c) =>
  (TExp a k -> Comp ('TFun b c) k) ->
  TExp ('TProd a b) k ->
  Comp c k
uncurry f p = do
  x <- fst_pair p
  y <- snd_pair p
  g <- f x
  return $ TEApp g y

apply :: (Typeable a, Typeable b) => TExp ('TFun a b) k -> TExp a k -> Comp b k
apply f x = return $ TEApp f x

{-

(<*>) :: (Typeable a) => (Typeable b) => Comp ('TFun a b) k -> Comp a k -> Comp b k

(<$$>) :: (Typeable a) => (Typeable b) => TExp ('TFun a b) k -> Comp a k -> Comp b k
(<$$>) f a = a >>= \_a -> apply f _a

liftA2 :: (Typeable a) => (Typeable b) => (Typeable c) => TExp ('TFun a ('TFun b c)) k -> Comp a k -> Comp b k -> Comp c k
liftA2 f x y = (f <$$> x) <*> y

-}

coerceToInt :: (PrimeField k) => TExp 'TField k -> Int
coerceToInt (TEVal (VField x)) = fromIntegral $ fromP x
coerceToInt _ = error "impossible: coerceToInt"

vec :: forall (n :: Nat) proxy ty k. (SNatI n) => proxy n -> Comp ('TVec n ty) k
vec _ = do
  let n = reflectToNum (Proxy @n)
  a <- arr n
  return $ unsafe_cast a

getV :: (Typeable ty) => (TExp ('TVec n ty) k, Fin n) -> Comp ty k
getV (a, i) =
  get (unsafe_cast a, fromIntegral $ toNatural i)

getV2 :: (Typeable ty) => (TExp ('TVec n ('TVec m ty)) k, Fin n, Fin m) -> Comp ty k
getV2 (a, i, j) =
  get2 (unsafe_cast a, fromIntegral $ toNatural i, fromIntegral $ toNatural j)

setV :: (Typeable ty) => (TExp ('TVec n ty) k, Fin n) -> TExp ty k -> Comp 'TUnit k
setV (a, i) e =
  set (unsafe_cast a, fromIntegral $ toNatural i) e

setV2 :: (Typeable ty) => (TExp ('TVec n ('TVec m ty)) k, Fin n, Fin m) -> TExp ty k -> Comp 'TUnit k
setV2 (a, i, j) e =
  set2 (unsafe_cast a, fromIntegral $ toNatural i, fromIntegral $ toNatural j) e

mapV :: forall a b (n :: Nat) k. (SNatI n) => (Typeable a) => (Typeable b) => TExp ('TFun a b) k -> TExp ('TVec n a) k -> Comp ('TVec n b) k
mapV f a = do
  b <- vec (Proxy @n)
  _ <- forall (universe @n) $ \i -> do
    ai <- getV (a, i)
    bi <- apply f ai
    setV (b, i) bi
  return b

foldlV ::
  forall a b (n :: Nat) k.
  (SNatI n) =>
  (Typeable a) =>
  (Typeable b) =>
  TExp ('TFun b ('TFun a b)) k ->
  TExp b k ->
  TExp ('TVec n a) k ->
  Comp b k
foldlV f b0 as = do
  go (universe @n) b0
  where
    go ns acc = case ns of
      [] -> return acc
      (n : rest) -> do
        ai <- getV (as, n)
        let acc' = TEApp (TEApp f acc) ai
        go rest acc'

traverseV ::
  forall a b (n :: Nat) k.
  (SNatI n) =>
  (Typeable a) =>
  (Typeable b) =>
  (TExp a k -> Comp b k) ->
  TExp ('TVec n a) k ->
  Comp ('TVec n b) k
traverseV f as = do
  bs <- vec (Proxy @n)
  _ <- forall (universe @n) $ \i -> do
    ai <- getV (as, i)
    bi <- f ai
    setV (bs, i) bi
  return bs

traverseV_ ::
  forall a b (n :: Nat) k.
  (SNatI n) =>
  (Typeable a) =>
  (TExp a k -> Comp 'TUnit k) ->
  TExp ('TVec n a) k ->
  Comp 'TUnit k
traverseV_ f as = do
  forall (universe @n) $ \i -> do
    ai <- getV (as, i)
    f ai

concatV ::
  forall (n :: Nat) (m :: Nat) a k.
  (SNatI n) =>
  (SNatI m) =>
  (Typeable a) =>
  TExp ('TVec n ('TVec m a)) k ->
  Comp ('TVec (Mult n m) a) k
concatV asV = do
  let n = reflectToNum (Proxy @n)
      m = reflectToNum (Proxy @m)
      as = unsafe_cast asV
  bs :: TExp ('TArr a) k <- arr (n P.* m)
  _ <- forall2 ([0 .. dec n], [0 .. dec m]) $ \i j -> do
    ai <- get2 (as, i, j)
    let idx = (n P.* i) P.+ j
    set (bs, idx) ai
  return $ unsafe_cast bs

chunkV ::
  forall (n :: Nat) (m :: Nat) k ty.
  (Typeable ty) =>
  (SNatI n) =>
  (SNatI m) =>
  TExp ('TVec (Mult n m) ty) k ->
  Comp ('TVec n ('TVec m ty)) k
chunkV asV = do
  let n = reflectToNum (Proxy @n)
      m = reflectToNum (Proxy @m)
      as = unsafe_cast asV
      is = fromIntegral . toNatural <$> universe @n
      js = fromIntegral . toNatural <$> universe @n
  bs <- arr2 @ty n m
  _ <- forall2 (is, js) $ \i j -> do
    let idx = n P.* i P.+ j
    ai <- get (as, idx)
    set2 (bs, i, j) ai
  return $ unsafe_cast bs

transpose ::
  forall (n :: Nat) (m :: Nat) k ty.
  (Typeable ty) =>
  (SNatI n) =>
  (SNatI m) =>
  TExp ('TVec n ('TVec m ty)) k ->
  Comp ('TVec m ('TVec n ty)) k
transpose asV = do
  let n = reflectToNum (Proxy @n)
      m = reflectToNum (Proxy @m)
      as = unsafe_cast asV
      is = fromIntegral . toNatural <$> universe @n
      js = fromIntegral . toNatural <$> universe @n
  bs <- arr2 @ty m n
  _ <- forall2 (is, js) $ \i j -> do
    ai <- get2 (as, i, j)
    set2 (bs, j, i) ai
  return $ unsafe_cast bs

allV ::
  (SNatI n) =>
  TExp ('TVec n 'TBool) k ->
  Comp 'TBool k
allV as = do
  f <- lambda $ \acc ->
    lambda $ \x ->
      return $ acc && x
  foldlV f true as