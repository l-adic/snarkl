module Snarkl.Constraint.Solve
  ( solve,
  )
where

import Data.Field.Galois (GaloisField)
import qualified Data.Map as Map
import Data.Maybe
  ( isJust,
  )
import Snarkl.Common (Assgn, Var (..))
import Snarkl.Constraint.Constraints
  ( ConstraintSystem (cs_in_vars, cs_num_vars, cs_out_vars),
  )
import Snarkl.Constraint.Simplify (do_simplify)
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)

-- | Starting from an initial partial assignment [env], solve the
-- constraints [cs] and return the resulting complete assignment.
-- If the constraints are unsolvable from [env], report the first
-- constraint that is violated (under normal operation, this error
-- case should NOT occur).
solve ::
  (GaloisField a) =>
  -- | Constraints to be solved
  ConstraintSystem a ->
  -- | Initial assignment
  Assgn a ->
  -- | Resulting assignment
  Assgn a
solve cs env =
  let pinned_vars = cs_in_vars cs ++ cs_out_vars cs
      -- NOTE: This looks really bad, but actually the only time that solve is called
      -- is right after renumber_constraints, which ensures that the variables are
      -- numbered from 0 to n-1, where n is the number of variables.
      all_vars = Var <$> [0 .. cs_num_vars cs - 1]
      (assgn, cs') = do_simplify True env cs
   in if all_assigned all_vars assgn
        then assgn
        else
          failWith $
            ErrMsg
              ( "unassigned variables,\n  "
                  ++ show (unassigned all_vars assgn)
                  ++ ",\n"
                  ++ "in assignment context\n  "
                  ++ show assgn
                  ++ ",\n"
                  ++ "in pinned-variable context\n  "
                  ++ show pinned_vars
                  ++ ",\n"
                  ++ "in reduced-constraint context\n  "
                  ++ show cs'
                  ++ ",\n"
                  ++ "in constraint context\n  "
                  ++ show cs
              )
  where
    all_assigned vars0 assgn = all (is_mapped assgn) vars0
    is_mapped assgn x = isJust (Map.lookup x assgn)
    unassigned vars0 assgn = [x | x <- vars0, not $ is_mapped assgn x]
