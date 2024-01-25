{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Snarkl.Constraint.Simplify
  ( do_simplify,
  )
where

import Control.Monad.State (State, evalState, gets, when)
import Data.Field.Galois (GaloisField)
import Data.List (foldl')
import qualified Data.Set as Set
import Snarkl.Common (Assgn (Assgn), Var)
import Snarkl.Constraint.Constraints
  ( CoeffList (CoeffList, asList),
    Constraint (..),
    ConstraintSet,
    ConstraintSystem (cs_constraints, cs_out_vars, cs_public_in_vars),
    cadd,
    constraint_vars,
  )
import Snarkl.Constraint.SimplMonad
  ( SEnv (SEnv, solve_mode),
    SolveMode (JustSimplify, UseMagic),
    assgn_of_vars,
    bind_of_var,
    bind_var,
    root_of_var,
    unite_vars,
  )
import qualified Snarkl.Constraint.UnionFind as UF

----------------------------------------------------------------
--                         Substitution                       --
----------------------------------------------------------------

-- | Normalize constraint 'constr', by substituting roots/constants
-- for the variables that appear in the constraint. Note that, when
-- normalizing a multiplicative constraint, it may be necessary to
-- convert it into an additive constraint.
subst_constr ::
  (GaloisField a) =>
  Constraint a ->
  State (SEnv a) (Constraint a)
subst_constr !constr = case constr of
  CMagic !_ !xs !mf ->
    do
      solveMode <- gets solve_mode
      case solveMode of
        UseMagic ->
          do
            b <- mf xs
            if b
              then return $ cadd 0 []
              else return constr
        JustSimplify -> return constr
  CAdd a m ->
    do
      -- Variables resolvable to constants
      consts' <-
        mapM
          ( \(x, a0) ->
              do
                var_or_a <- bind_of_var x
                case var_or_a of
                  Left _ -> return []
                  Right a' -> return [(x, a0 * a')]
          )
          $! asList m
      let consts = concat consts'
      let const_keys = map fst consts
      let const_vals = map snd consts
      -- The new constant folding in all constant constraint variables
      let new_const = foldl' (+) a const_vals
      -- The linear combination minus
      -- (1) Terms whose variables resolve to constants, and
      -- (2) Terms with coeff 0.
      let less_consts =
            filter (\(k, v) -> notElem k const_keys && v /= 0) $!
              asList m
      -- The new linear combination: 'less_consts' with all variables
      -- replaced by their roots.
      new_map <-
        mapM
          ( \(x, a0) ->
              do
                rx <- root_of_var x
                return (rx, a0)
          )
          less_consts
      return $! cadd new_const new_map
  CMult (c, x) (d, y) !ez ->
    do
      bx <- bind_of_var x
      by <- bind_of_var y
      bz <- bind_of_term ez
      case (bx, by, bz) of
        (Left rx, Left ry, Left (e, rz)) ->
          return $!
            CMult (c, rx) (d, ry) (e, Just rz)
        (Left rx, Left ry, Right e) ->
          return $!
            CMult (c, rx) (d, ry) (e, Nothing)
        (Left rx, Right d0, Left (e, rz)) ->
          return $!
            cadd 0 [(rx, c * d * d0), (rz, -e)]
        (Left rx, Right d0, Right e) ->
          return $!
            cadd (-e) [(rx, c * d * d0)]
        (Right c0, Left ry, Left (e, rz)) ->
          return $!
            cadd 0 [(ry, c0 * c * d), (rz, -e)]
        (Right c0, Left ry, Right e) ->
          return $!
            cadd (-e) [(ry, c0 * c * d)]
        (Right c0, Right d0, Left (e, rz)) ->
          return $!
            cadd (c * c0 * d * d0) [(rz, -e)]
        (Right c0, Right d0, Right e) ->
          return $!
            cadd ((c * c0 * d * d0) - e) []
    where
      bind_of_term (e, Nothing) =
        return $ Right e
      bind_of_term (e, Just z) =
        do
          var_or_a <- bind_of_var z
          case var_or_a of
            Left rz -> return $ Left (e, rz)
            Right e0 -> return $ Right (e * e0)

----------------------------------------------------------------
--                 Constraint Set Minimization                --
----------------------------------------------------------------

-- | Is 'constr' a tautology?
is_taut ::
  Constraint a ->
  State (SEnv a) Bool
is_taut constr =
  case constr of
    CAdd _ (CoeffList []) -> return True
    CAdd _ (CoeffList (_ : _)) -> return False
    CMult {} -> return False
    CMagic _ xs mf -> mf xs

-- | Remove tautologous constraints.
remove_tauts :: (GaloisField a) => [Constraint a] -> State (SEnv a) [Constraint a]
remove_tauts sigma =
  do
    sigma_taut <-
      mapM
        ( \t -> do
            t' <- subst_constr t
            b <- is_taut t'
            return (b, t')
        )
        sigma
    return $ map snd $ filter (not . fst) sigma_taut

-- | Learn bindings and variable equalities from constraint 'constr'.
learn ::
  (GaloisField a) =>
  Constraint a ->
  State (SEnv a) ()
learn = go
  where
    go (CAdd a (CoeffList [(x, c)])) =
      if c == 0
        then return ()
        else bind_var (x, -a * recip c)
    go (CAdd a (CoeffList [(x, c), (y, d)]))
      | a == 0 = when (c == -d) (unite_vars x y)
    go (CAdd _ _) =
      return ()
    go _ = return ()

do_simplify ::
  (GaloisField a) =>
  -- | Snarkl.Constraint.Solve mode? If 'True', use Magic.
  Bool ->
  -- | Initial variable assignment
  Assgn a ->
  -- | Constraint set to be simplified
  ConstraintSystem a ->
  -- | Resulting assignment, simplified constraint set
  (Assgn a, ConstraintSystem a)
do_simplify in_solve_mode (Assgn env) cs =
  -- NOTE: Pinned vars include:
  --   - input vars
  --   - output vars
  --   - magic vars (those that appear in magic constraints, used to
  --   resolve nondeterministic inputs)
  -- Pinned vars are never optimized away.
  let pinned_vars = cs_public_in_vars cs ++ cs_out_vars cs ++ magic_vars (cs_constraints cs)
      do_solve = if in_solve_mode then UseMagic else JustSimplify
      new_state = SEnv (UF.empty {UF.extras = env}) do_solve
   in evalState (go pinned_vars) new_state
  where
    go pinned_vars =
      do
        sigma' <- simplify pinned_vars $ cs_constraints cs
        -- NOTE: In the next line, it's OK that 'pinned_vars'
        -- may overlap with 'constraint_vars cs'.
        -- 'assgn_of_vars' might do a bit of duplicate
        -- work (to look up the same key more than once).
        assgn <-
          assgn_of_vars $
            pinned_vars
              ++ constraint_vars (cs_constraints cs)
        return (assgn, cs {cs_constraints = sigma'})
    magic_vars cs0 =
      Set.fold
        ( \c0 acc ->
            case c0 of
              CMagic _ xs _ -> xs ++ acc
              _ -> acc
        )
        []
        cs0

simplify ::
  (GaloisField a) =>
  [Var] ->
  ConstraintSet a ->
  State (SEnv a) (ConstraintSet a)
simplify pinned_vars sigma =
  do
    sigma' <- simplify_rec sigma
    sigma_subst <- mapM subst_constr $ Set.toList sigma'
    sigma_no_tauts <- remove_tauts sigma_subst
    sigma_pinned <- add_pin_eqns sigma_no_tauts
    return $ Set.fromList sigma_pinned
  where
    -- NOTE: We handle pinned variables 'x' as follows:
    --  (1) Look up the term associated with
    --      the pinned variable, if any (call it 't').
    --  (2) If there is no such term (other than 'x' itself),
    --      do nothing (clauses containing the pinned
    --      variable must still contain the pinned variable).
    --  (3) Otherwise, introduce a new equation 'x = t'.
    add_pin_eqns sigma0 =
      do
        pinned_terms <-
          mapM
            ( \x -> do
                var_or_a <- bind_of_var x
                return (x, var_or_a)
            )
            pinned_vars
        let pin_eqns =
              map
                ( \(x, var_or_a) ->
                    case var_or_a of
                      Left rx ->
                        cadd 0 [(x, 1), (rx, -1)]
                      Right c ->
                        cadd (-c) [(x, 1)]
                )
                $ filter (\(x, rx) -> Left x /= rx) pinned_terms
        return $ pin_eqns ++ sigma0

simplify_rec ::
  (GaloisField a) =>
  -- | Initial constraint set
  ConstraintSet a ->
  -- | Resulting simplified constraint set
  State (SEnv a) (ConstraintSet a)
simplify_rec sigma =
  do
    sigma' <- simplify_once sigma
    if Set.size sigma' < Set.size sigma
      then simplify_rec sigma'
      else
        if Set.difference sigma sigma'
          `Set.isSubsetOf` Set.empty
          then return sigma'
          else simplify_rec sigma'
  where
    simplify_once ::
      (GaloisField a) =>
      ConstraintSet a ->
      -- \^ Initial constraint set
      State (SEnv a) (ConstraintSet a)
    -- \^ Resulting simplified constraint set
    simplify_once sigma0 =
      do
        sigma2 <- go Set.empty sigma0
        sigma' <- remove_tauts (Set.toList sigma2)
        return $ Set.fromList sigma'

    go ws us
      | Set.size us == 0 =
          return ws
    go ws us =
      let (given, us') = choose us
       in do
            given' <- subst_constr given
            given_taut <- is_taut given'
            if given_taut
              then go ws us'
              else do
                learn given'
                let ws' = Set.insert given' ws
                go ws' us'

    -- NOTE: Assumes input set is nonempty
    choose s = Set.deleteFindMin s
