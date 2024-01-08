module Snarkl.Language
  ( compileTExpToProgram,
    -- | Snarkl.Language.TExpr,
    booleanVarsOfTexp,
    TExp,
    -- | Snarkl.Language.Core,
    Variable (..),
    Program (..),
    Assignment (..),
    Exp (..),
    -- types
    module Snarkl.Language.Type,
    -- | SyntaxMonad and Syntax
    Comp,
    runState,
    return,
    (>>=),
    (>>),
    Env (..),
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
import Snarkl.Language.Core
  ( Assignment (..),
    Exp (..),
    Program (..),
    Variable (..),
  )
import Snarkl.Language.Expr (mkProgram)
import Snarkl.Language.LambdaExpr (expOfLambdaExp)
import Snarkl.Language.Syntax
import Snarkl.Language.SyntaxMonad
import Snarkl.Language.TExpr (TExp, booleanVarsOfTexp, tExpToLambdaExp)
import Snarkl.Language.Type
import Prelude (Either (..), error, ($), (.), (<>))

compileTExpToProgram :: (GaloisField a, Typeable ty) => TExp ty a -> Program a
compileTExpToProgram te =
  let eprog = mkProgram . expOfLambdaExp . tExpToLambdaExp $ te
   in case eprog of
        Right p -> p
        Left err -> error $ "compileTExpToProgram: failed to convert TExp to Program: " <> err