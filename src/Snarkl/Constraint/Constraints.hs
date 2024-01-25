{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Snarkl.Constraint.Constraints
  ( CoeffList (..),
    coeff_insert,
    Constraint (..),
    cadd,
    ConstraintSet,
    ConstraintSystem (..),
    SimplifiedConstraintSystem (..),
    r1cs_of_cs,
    renumber_constraints,
    constraint_vars,
  )
where

import Control.Monad.State (State)
import qualified Data.Aeson as A
import Data.Bifunctor (Bifunctor (first, second))
import Data.Field.Galois (GaloisField (char, deg), Prime, PrimeField)
import Data.JSONLines (FromJSONLines (fromJSONLines), ToJSONLines (toJSONLines), WithHeader (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Snarkl.Backend.R1CS (Poly (Poly), R1C (R1C), R1CS (R1CS), const_poly, var_poly)
import Snarkl.Common (Assgn (Assgn), ConstraintHeader (..), FieldElem (..), Var (Var))
import Snarkl.Constraint.SimplMonad (SEnv)
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)

----------------------------------------------------------------
--            Intermediate Constraint Language
----------------------------------------------------------------

newtype CoeffList k v = CoeffList {asList :: [(k, v)]}
  deriving (Eq)

instance (PrimeField v) => A.ToJSON (CoeffList Var v) where
  toJSON (CoeffList l) = A.toJSON $ map (second FieldElem) l

instance (PrimeField v) => A.FromJSON (CoeffList Var v) where
  parseJSON v = CoeffList . map (second unFieldElem) <$> A.parseJSON v

-- COEFFLIST INVARIANT: no key appears more than once.  Upon duplicate
-- insertion, insert field sum of the values.  Terms with 0 coeff. are
-- implicitly removed. Smart constructor 'cadd' (below) enforces this
-- invariant.

coeff_insert :: (Eq k, GaloisField a) => k -> a -> CoeffList k a -> CoeffList k a
coeff_insert k a l = CoeffList $ go (asList l)
  where
    go [] = [(k, a)]
    go (scrut@(k', a') : l') =
      if k == k'
        then (k, a + a') : l'
        else scrut : go l'

coeff_merge :: (Eq k, GaloisField a) => CoeffList k a -> CoeffList k a
coeff_merge l = go (CoeffList []) (asList l)
  where
    go acc [] = acc
    go acc ((k, a) : l') =
      go (coeff_insert k a acc) l'

remove_zeros :: (GaloisField a) => CoeffList k a -> CoeffList k a
remove_zeros (CoeffList l) = CoeffList $ go [] l
  where
    go acc [] = acc
    go acc ((_, a) : l')
      | a == 0 =
          go acc l'
    go acc (scrut@(_, _) : l') =
      go (scrut : acc) l'

-- | Constraints are either
--   * 'CAdd a m': A linear combination of the constant 'a' with
--     the variable-coeff. terms given by map 'm : Map.Map Var a'.
--   * 'CMult (c,x) (d,y) (e,mz)': A multiplicative constraint with
--     interpretation cx * dy = e (when mz = Nothing), or
--                    cx * dy = ez (when mz = Just z).
data Constraint a
  = CAdd !a !(CoeffList Var a)
  | CMult !(a, Var) !(a, Var) !(a, Maybe Var)
  | CMagic Var [Var] ([Var] -> State (SEnv a) Bool)

instance (PrimeField a) => A.ToJSON (Constraint a) where
  toJSON (CAdd a m) =
    A.object
      [ "tag" A..= ("CAdd" :: String),
        "data"
          A..= A.object
            [ "a" A..= FieldElem a,
              "m" A..= m
            ]
      ]
  toJSON (CMult (c, x) (d, y) (e, mz)) =
    A.object
      [ "tag" A..= ("CMult" :: String),
        "data"
          A..= A.object
            [ "cx" A..= (FieldElem c, x),
              "dy" A..= (FieldElem d, y),
              "ez" A..= (FieldElem e, mz)
            ]
      ]
  toJSON (CMagic {}) = error "ToJSON (Constraint a): CMagic not implemented"

instance (PrimeField a) => A.FromJSON (Constraint a) where
  parseJSON =
    A.withObject "Constraint" $ \v -> do
      tag <- v A..: "tag"
      case tag :: String of
        "CAdd" -> do
          d <- v A..: "data"
          a <- d A..: "a"
          m <- d A..: "m"
          pure $ CAdd (unFieldElem a) m
        "CMult" -> do
          d <- v A..: "data"
          cx <- d A..: "cx"
          dy <- d A..: "dy"
          ez <- d A..: "ez"
          pure $ CMult (first unFieldElem cx) (first unFieldElem dy) (first unFieldElem ez)
        _ -> error "FromJSON (Constraint a): unknown tag"

-- | Smart constructor enforcing CoeffList invariant
cadd :: (GaloisField a) => a -> [(Var, a)] -> Constraint a
cadd !a !l = CAdd a (remove_zeros $ coeff_merge $ CoeffList l)

type ConstraintSet a = Set.Set (Constraint a)

data ConstraintSystem a = ConstraintSystem
  { cs_constraints :: ConstraintSet a,
    cs_num_vars :: Int,
    cs_public_in_vars :: [Var],
    cs_out_vars :: [Var]
  }
  deriving (Show, Eq)

instance (Eq a) => Eq (Constraint a) where
  CAdd c m == CAdd c' m' =
    c == c' && m == m'
  CMult cx dy emz == CMult cx' dy' emz' =
    emz == emz'
      && (cx == cx' && dy == dy' || cx == dy' && dy == cx')
  CMagic nm _ _ == CMagic nm' _ _ = nm == nm'
  CAdd _ _ == CMult {} = False
  CMult {} == CAdd _ _ = False
  CMagic {} == _ = False
  _ == CMagic {} = False

compare_add :: (Ord a) => Constraint a -> Constraint a -> Ordering
{-# INLINE compare_add #-}
compare_add (CAdd c m) (CAdd c' m')
  | c == c' = compare (asList m) (asList m')
  | c < c' = LT
  | otherwise = GT
compare_add !_ !_ =
  failWith $ ErrMsg "internal error: compare_add"

compare_mult :: (Ord a) => Constraint a -> Constraint a -> Ordering
{-# INLINE compare_mult #-}
compare_mult
  (CMult (c, x) (d, y) (e, mz))
  (CMult (c', x') (d', y') (e', mz'))
    | x == x' =
        if y == y'
          then case compare mz mz' of
            EQ -> case compare c c' of
              EQ -> case compare d d' of
                EQ -> compare e e'
                other -> other
              other -> other
            other -> other
          else if y < y' then LT else GT
    | x < x' = LT
    | otherwise = GT
compare_mult !_ !_ =
  failWith $ ErrMsg "internal error: compare_mult"

compare_constr :: (Ord a) => Constraint a -> Constraint a -> Ordering
{-# INLINE compare_constr #-}
compare_constr (CAdd _ _) (CMult {}) = LT
compare_constr (CMult {}) (CAdd _ _) = GT
compare_constr constr@(CAdd _ _) constr'@(CAdd _ _) =
  compare_add constr constr'
compare_constr constr@(CMult {}) constr'@(CMult {}) =
  compare_mult constr constr'
compare_constr (CMagic nm _ _) (CMagic nm' _ _) = compare nm nm'
compare_constr !_ (CMagic {}) = LT
compare_constr (CMagic {}) !_ = GT

instance (Ord a) => Ord (Constraint a) where
  {-# SPECIALIZE instance Ord (Constraint (Prime p)) #-}
  compare = compare_constr

instance (Show a) => Show (Constraint a) where
  show (CAdd a m) = show a ++ " + " ++ go (asList m)
    where
      go [] = " == 0"
      go [(x, c)] = show c ++ "x" ++ show x ++ go []
      go ((x, c) : c_xs) = show c ++ "x" ++ show x ++ " + " ++ go c_xs
  show (CMult (c, x) (d, y) (e, mz)) =
    let show_term c0 x0 = show c0 ++ "x" ++ show x0
     in show_term c x
          ++ " * "
          ++ show_term d y
          ++ " == "
          ++ case mz of
            Nothing -> show e
            Just z -> show_term e z
  show (CMagic nm xs _) = "Magic " ++ show (nm, xs)

----------------------------------------------------------------
-- Compilation to R1CS
----------------------------------------------------------------

-- Contains no magic constraints, hence we can serialize them
newtype SimplifiedConstraintSystem a = SimplifiedConstraintSystem
  { unSimplifiedConstraintSystem :: ConstraintSystem a
  }
  deriving (Show, Eq)

instance (PrimeField k) => ToJSONLines (SimplifiedConstraintSystem k) where
  toJSONLines scs@(SimplifiedConstraintSystem (ConstraintSystem {..})) =
    toJSONLines $ WithHeader (constraintSystemHeader scs) (Set.toList cs_constraints)
    where
      constraintSystemHeader (_ :: SimplifiedConstraintSystem a) =
        ConstraintHeader
          { field_characteristic = toInteger $ char (undefined :: a),
            extension_degree = toInteger $ deg (undefined :: a),
            n_constraints = Set.size cs_constraints,
            n_variables = cs_num_vars,
            input_variables = cs_public_in_vars,
            output_variables = cs_out_vars
          }

instance (PrimeField k) => FromJSONLines (SimplifiedConstraintSystem k) where
  fromJSONLines ls = do
    WithHeader ConstraintHeader {..} cs <- fromJSONLines ls
    pure $
      SimplifiedConstraintSystem $
        ConstraintSystem
          { cs_constraints = Set.fromList cs,
            cs_num_vars = fromIntegral n_variables,
            cs_public_in_vars = input_variables,
            cs_out_vars = output_variables
          }

r1cs_of_cs ::
  (GaloisField a) =>
  -- | Constraints
  SimplifiedConstraintSystem a ->
  R1CS a
r1cs_of_cs (SimplifiedConstraintSystem cs) =
  R1CS
    (go $ Set.toList $ cs_constraints cs)
    (cs_num_vars cs)
    (cs_public_in_vars cs)
    (cs_out_vars cs)
  where
    go [] = []
    go (CAdd a m : cs') =
      R1C
        ( const_poly 1,
          Poly $ Assgn $ Map.insert (Var (-1)) a $ Map.fromList (asList m),
          const_poly 0
        )
        : go cs'
    go (CMult cx dy (e, Nothing) : cs') =
      R1C (var_poly cx, var_poly dy, const_poly e) : go cs'
    go (CMult cx dy (e, Just z) : cs') =
      R1C (var_poly cx, var_poly dy, var_poly (e, z)) : go cs'
    go (CMagic {} : cs') =
      go cs'

-- | Return the list of variables occurring in constraints 'cs'.
constraint_vars :: ConstraintSet a -> [Var]
constraint_vars cs =
  Set.toList $
    Set.foldl' (\s0 c -> Set.union (get_vars c) s0) Set.empty cs
  where
    get_vars (CAdd _ m) = Set.fromList $ map fst (asList m)
    get_vars (CMult (_, x) (_, y) (_, Nothing)) = Set.fromList [x, y]
    get_vars (CMult (_, x) (_, y) (_, Just z)) = Set.fromList [x, y, z]
    get_vars (CMagic _ xs _) = Set.fromList xs

-- | Sequentially renumber term variables '0..max_var'.  Return
--   renumbered constraints, together with the total number of
--   variables in the (renumbered) constraint set and the (possibly
--   renumbered) in and out variables.
renumber_constraints ::
  (GaloisField a) =>
  ConstraintSystem a ->
  ( Var -> Var,
    ConstraintSystem a
  )
renumber_constraints cs =
  (renum_f, ConstraintSystem new_cs (Map.size var_map) new_in_vars new_out_vars)
  where
    new_cs = Set.map renum_constr $ cs_constraints cs
    new_in_vars = map renum_f $ cs_public_in_vars cs
    new_out_vars = map renum_f $ cs_out_vars cs

    var_map =
      Map.fromList $
        zip (cs_public_in_vars cs <> filter isnt_input all_vars) (Var <$> [0 ..])
      where
        isnt_input = not . flip Set.member in_vars_set
        in_vars_set = Set.fromList $ cs_public_in_vars cs
        all_vars = constraint_vars $ cs_constraints cs

    renum_f x =
      case Map.lookup x var_map of
        Nothing ->
          failWith $
            ErrMsg
              ( "can't find a binding for variable "
                  ++ show x
                  ++ " in map "
                  ++ show var_map
              )
        Just x' -> x'

    renum_constr c0 =
      case c0 of
        CAdd a m ->
          cadd a $ map (first renum_f) (asList m)
        CMult (c, x) (d, y) (e, mz) ->
          CMult (c, renum_f x) (d, renum_f y) (e, fmap renum_f mz)
        CMagic nm xs f ->
          CMagic nm (map renum_f xs) f
