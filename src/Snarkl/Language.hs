module Snarkl.Language
  ( expOfTExp,

    -- * Snarkl.Language.TExpr
    TExp,
    booleanVarsOfTexp,

    -- * Snarkl.Language.Expr
    Exp (..),
    Variable (..),
    do_const_prop,
    var_of_exp,

    -- * Snarkl.Language.SyntaxMonad
    Comp,
    Env (Env, input_vars, next_variable),
    runState,

    -- * Types
    module Snarkl.Language.Type,
  )
where

import Data.Data (Typeable)
import Data.Field.Galois (GaloisField)
import Snarkl.Language.Expr (Exp (..), Variable (..), do_const_prop, var_of_exp)
import Snarkl.Language.LambdaExpr (expOfLambdaExp)
import Snarkl.Language.SyntaxMonad (Comp, Env (Env, input_vars, next_variable), runState)
import Snarkl.Language.TExpr (TExp, booleanVarsOfTexp, lambdaExpOfTExp)
import Snarkl.Language.Type

expOfTExp :: (GaloisField a, Typeable ty) => TExp ty a -> Exp a
expOfTExp = expOfLambdaExp . lambdaExpOfTExp
