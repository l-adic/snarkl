module Snarkl.AST
  ( expOfTExp,

    -- * Snarkl.AST.TExpr
    TExp,
    booleanVarsOfTexp,

    -- * Snarkl.AST.Expr
    Exp (..),
    Variable (..),
    do_const_prop,
    var_of_exp,

    -- * Snarkl.AST.SyntaxMonad
    Comp,
    Env (Env, input_vars, next_variable),
    InputVariable (..),
    defaultEnv,
    runComp,

    -- * Types
    module Snarkl.AST.Type,
  )
where

import Data.Data (Typeable)
import Data.Field.Galois (GaloisField)
import Snarkl.AST.Expr (Exp (..), Variable (..), do_const_prop, var_of_exp)
import Snarkl.AST.LambdaExpr (expOfLambdaExp)
import Snarkl.AST.SyntaxMonad (Comp, Env (Env, input_vars, next_variable), InputVariable (..), defaultEnv, runComp)
import Snarkl.AST.TExpr (TExp, booleanVarsOfTexp, lambdaExpOfTExp)
import Snarkl.AST.Type

expOfTExp :: (GaloisField a, Typeable ty) => TExp ty a -> Exp a
expOfTExp = expOfLambdaExp . lambdaExpOfTExp
