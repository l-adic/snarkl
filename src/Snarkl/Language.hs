module Snarkl.Language
  ( expOfTExp,
    module Snarkl.Language.TExpr,
    module Snarkl.Language.Expr,
    module Snarkl.Language.SyntaxMonad,
    module Snarkl.Language.Syntax,
  )
where

import Data.Data (Typeable)
import Snarkl.Field (Field)
import Snarkl.Language.Expr
import Snarkl.Language.LambdaExpr (expOfLambdaExp)
import Snarkl.Language.Syntax
import Snarkl.Language.SyntaxMonad
import Snarkl.Language.TExpr

expOfTExp :: (Field a, Typeable ty) => TExp ty a -> Exp a
expOfTExp = expOfLambdaExp . lambdaExpOfTExp
