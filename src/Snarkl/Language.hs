module Snarkl.Language
  ( expOfTExp,
    module Snarkl.Language.TExpr,
    module Snarkl.Language.Expr,
    module Snarkl.Language.SyntaxMonad,
    module Snarkl.Language.Syntax,
  )
where

import Data.Data (Typeable)
import Data.Field.Galois (GaloisField)
import Snarkl.Language.Expr
import Snarkl.Language.LambdaExpr (expOfLambdaExp)
import Snarkl.Language.Syntax
import Snarkl.Language.SyntaxMonad
import Snarkl.Language.TExpr

expOfTExp :: (GaloisField a, Typeable ty) => TExp ty a -> Exp a
expOfTExp = expOfLambdaExp . lambdaExpOfTExp
