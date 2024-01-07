module Snarkl.Language
  ( expOfTExp,
    booleanVarsOfTexp,
    TExp,
    module Snarkl.Language.Expr,
    -- | SyntaxMonad
    Comp,
    runState,
    return,
    (>>=),
    (>>),
    Env (..),
    -- | Return a fresh input variable.
    InputType (..),
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
  )
where

import Data.Data (Typeable)
import Data.Field.Galois (GaloisField)
import Snarkl.Language.Expr
import Snarkl.Language.LambdaExpr (expOfLambdaExp)
import Snarkl.Language.Syntax
import Snarkl.Language.SyntaxMonad
import Snarkl.Language.TExpr
import qualified Prelude

expOfTExp :: (GaloisField a, Typeable ty) => TExp ty a -> Exp a
expOfTExp = expOfLambdaExp Prelude.. lambdaExpOfTExp
