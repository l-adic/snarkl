{-# LANGUAGE NamedFieldPuns #-}

module Snarkl.Constraint.Dataflow (removeUnreachable) where

import Control.Monad.State
  ( State,
    evalState,
    gets,
    modify,
  )
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Snarkl.Common (Var)
import Snarkl.Constraint.Constraints
  ( Constraint,
    ConstraintSystem (cs_constraints, cs_out_vars),
    constraint_vars,
  )

newtype ConstraintId = ConstraintId Int
  deriving (Eq, Ord)

-- | Give a unique number (i.e. and index) to each constraint
numberConstraints :: ConstraintSystem a -> Map ConstraintId (Constraint a)
numberConstraints cs =
  foldl' (\m (i, c) -> Map.insert (ConstraintId i) c m) Map.empty (zip [0 ..] (Set.toList $ cs_constraints cs))

-- | Creates a mapping from variables to the set of constraint IDs in which each variable appears.
-- Implicitly uses the type alias 'Var' for the keys.
gatherVars :: Map ConstraintId (Constraint a) -> Map Var (Set ConstraintId)
gatherVars constr_map =
  let f m (cid, constr) =
        let vars = constraint_vars (Set.singleton constr)
         in foldl' (\acc x -> addVar x cid acc) m vars
   in foldl' f Map.empty (Map.toList constr_map)
  where
    addVar :: Var -> ConstraintId -> Map Var (Set ConstraintId) -> Map Var (Set ConstraintId)
    addVar x cid m =
      case Map.lookup x m of
        Nothing -> Map.insert x (Set.singleton cid) m
        Just s0 -> Map.insert x (Set.insert cid s0) m

newtype Roots a = DEnv {dfRoots :: Set Var}

addRoot :: Var -> State (Roots a) ()
addRoot x = modify (\s -> s {dfRoots = Set.insert x (dfRoots s)})

data Env a = Env
  { constraints :: Map ConstraintId (Constraint a),
    varMap :: Map Var (Set ConstraintId)
  }

-- |  Removes constraints that are unreachable from the output variables of a ConstraintSystem.
removeUnreachable ::
  (Ord a) =>
  ConstraintSystem a ->
  ConstraintSystem a
removeUnreachable cs =
  let nConstrs = numberConstraints cs
      env =
        Env
          { constraints = nConstrs,
            varMap = gatherVars nConstrs
          }
      foundConstraints =
        flip evalState (DEnv Set.empty) $
          do
            mapM_ addRoot (cs_out_vars cs)
            -- start searching from the output variables
            exploreVars env (cs_out_vars cs)
            roots <- gets dfRoots
            pure $ lookupConstraints env roots
   in cs {cs_constraints = foundConstraints}
  where
    lookupConstraints :: (Ord a) => Env a -> Set Var -> Set (Constraint a)
    lookupConstraints Env {constraints, varMap} roots =
      Set.foldl
        ( \s x ->
            case Map.lookup x varMap of
              Nothing -> s
              Just constraintIds ->
                getConstraints constraintIds `Set.union` s
        )
        Set.empty
        roots
      where
        getConstraints =
          Set.foldl
            ( \s cid ->
                case Map.lookup cid constraints of
                  Nothing -> s
                  Just constr -> Set.insert constr s
            )
            Set.empty

exploreVars ::
  Env a ->
  [Var] ->
  State (Roots a) ()
exploreVars _ [] = pure ()
exploreVars env@(Env {constraints, varMap}) (r : rest) =
  case Map.lookup r varMap of
    Nothing -> exploreVars env rest
    Just cids ->
      do
        let vars = getVars (Set.toList cids)
        currentRoots <- gets dfRoots
        let newRoots = filter (\v -> not $ Set.member v currentRoots) vars
        mapM_ addRoot newRoots
        exploreVars env (newRoots <> rest)
  where
    getVars :: [ConstraintId] -> [Var]
    getVars vars =
      let f cid =
            case Map.lookup cid constraints of
              Nothing -> []
              Just constr -> constraint_vars (Set.singleton constr)
       in concatMap f vars
