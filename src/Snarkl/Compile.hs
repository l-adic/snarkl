{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Snarkl.Compile
  ( TExpPkg (..),
    SimplParam (..),
    compileConstraintsToR1CS,
    compileTExpToR1CS,
    compileCompToR1CS,
    compileCompToTexp,
    compileTexpToConstraints,
    compileCompToConstraints,
  )
where

import Control.Lens (Iso', iso, view, (#), (^.))
import Control.Monad.State
  ( State,
    gets,
    modify,
  )
import qualified Control.Monad.State as State
import Data.Either (fromRight)
import Data.Field.Galois (GaloisField)
-- do_const_prop,

import Data.Foldable (traverse_)
import Data.List (sort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Prettyprinter (Pretty (..))
import Snarkl.Backend.R1CS.R1CS (R1CS)
import Snarkl.Common (Op (..), UnOp (..), Var (Var), incVar)
import Snarkl.Constraint
  ( Constraint (CMagic, CMult),
    ConstraintSystem (ConstraintSystem),
    bind_of_var,
    bind_var,
    cadd,
    constraint_vars,
    do_simplify,
    r1cs_of_cs,
    removeUnreachable,
    renumber_constraints,
    solve,
  )
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Language
  ( Comp,
    Env (Env, input_vars, next_variable),
    TExp,
    Variable (Variable),
    booleanVarsOfTexp,
    expOfTExp,
    runState,
  )
import qualified Snarkl.Language.Core as Core

----------------------------------------------------------------
--
-- Expr -> Constraints
--
----------------------------------------------------------------

data CEnv a = CEnv
  { cur_cs :: Set.Set (Constraint a),
    next_var :: Var
  }

add_constraint :: (Ord a) => Constraint a -> State (CEnv a) ()
add_constraint c =
  modify (\cenv -> cenv {cur_cs = Set.insert c $ cur_cs cenv})

get_constraints :: State (CEnv a) [Constraint a]
get_constraints = gets (Set.toList . cur_cs)

get_next_var :: State (CEnv a) Var
get_next_var = gets next_var

set_next_var :: Var -> State (CEnv a) ()
set_next_var next = modify (\cenv -> cenv {next_var = next})

fresh_var :: State (CEnv a) Var
fresh_var =
  do
    next <- get_next_var
    set_next_var (incVar next)
    return next

-- | Add constraint 'x = y'
ensure_equal :: (GaloisField a) => (Var, Var) -> State (CEnv a) ()
ensure_equal (x, y) =
  add_constraint $
    cadd 0 [(x, 1), (y, -1)]

-- | Add constraint 'x = c'
ensure_const :: (GaloisField a) => (Var, a) -> State (CEnv a) ()
ensure_const (x, c) =
  add_constraint $
    cadd c [(x, -1)]

-- | Add constraint 'b^2 = b'.
ensure_boolean :: (GaloisField a) => Var -> State (CEnv a) ()
ensure_boolean b =
  encode_binop Mult (b, b, b)

-- | Constraint 'x \/ y = z'.
-- The encoding is: x+y - z = x*y; assumes x and y are boolean.
encode_or :: (GaloisField a) => (Var, Var, Var) -> State (CEnv a) ()
encode_or (x, y, z) =
  do
    x_mult_y <- fresh_var
    cs_of_exp x_mult_y (Core.EBinop Mult [Core.EVar (_Var # x), Core.EVar (_Var # y)])
    cs_of_exp
      x_mult_y
      ( Core.EBinop
          Sub
          [ Core.EBinop Add [Core.EVar (_Var # x), Core.EVar (_Var # y)],
            Core.EVar (_Var # z)
          ]
      )

-- | Constraint 'x xor y = z'.
-- The encoding is: x+y - z = 2(x*y); assumes x and y are boolean.
encode_xor :: (GaloisField a) => (Var, Var, Var) -> State (CEnv a) ()
encode_xor (x, y, z) =
  do
    x_mult_y <- fresh_var
    encode_binop Mult (x, y, x_mult_y)
    add_constraint $
      cadd
        0
        [ (x, 1),
          (y, 1),
          (z, -1),
          (x_mult_y, -2)
        ]

-- -- The following desugaring is preferable, but generates more constraints.
-- -- Perhaps something to investigate wrt. Simplify.hs.
--   = do { x_mult_y <- fresh_var
--        ; cs_of_exp x_mult_y (EBinop Mult
--                                     [EVal (one `add` one)
--                                     ,EBinop Mult [EVar x,EVar y]])
--        ; cs_of_exp x_mult_y (EBinop Sub
--                                     [EBinop Add [EVar x,EVar y]
--                                     ,EVar z])
--        }

-- | Constraint 'x == y = z' ASSUMING x, y are boolean.
-- The encoding is: x*y + (1-x)*(1-y) = z.
encode_boolean_eq :: (GaloisField a) => (Var, Var, Var) -> State (CEnv a) ()
encode_boolean_eq (x, y, z) = cs_of_exp z e
  where
    e =
      Core.EBinop
        Add
        [ Core.EBinop Mult [Core.EVar (_Var # x), Core.EVar (_Var # y)],
          Core.EBinop
            Mult
            [ Core.EBinop Sub [Core.EVal 1, Core.EVar (_Var # x)],
              Core.EBinop Sub [Core.EVal 1, Core.EVar (_Var # y)]
            ]
        ]

-- | Constraint 'x == y = z'.
-- The encoding is: z = (x-y == 0).
encode_eq :: (GaloisField a) => (Var, Var, Var) -> State (CEnv a) ()
encode_eq (x, y, z) =
  cs_of_assignment $
    Core.Assignment
      (_Var # z)
      (Core.EUnop ZEq (Core.EBinop Sub [Core.EVar (_Var # x), Core.EVar (_Var # y)]))

-- | Constraint 'y = x!=0 ? 1 : 0'.
-- The encoding is:
-- for some m,
--    x*m = y
-- /\ (1-y)*x = 0
-- Cf. p7. of [pinnochio-sp13], which follows [setty-usenix12].
encode_zneq :: (GaloisField a) => (Var, Var) -> State (CEnv a) ()
encode_zneq (x, y) =
  do
    m <- fresh_var
    neg_y <- fresh_var
    -- The following 'magic' constraint resolves the value of
    -- nondet witness 'm':
    --   m = 0,      x = 0
    --   m = inv x,  x <> 0
    nm <- fresh_var
    add_constraint (CMagic nm [x, m] mf)
    -- END magic.
    cs_of_exp y (Core.EBinop Mult [Core.EVar (_Var # x), Core.EVar (_Var # m)])
    cs_of_exp neg_y (Core.EBinop Sub [Core.EVal 1, Core.EVar (_Var # y)])
    add_constraint
      (CMult (1, neg_y) (1, x) (0, Nothing))
  where
    mf [x0, m0] =
      do
        tx <- bind_of_var x0
        case tx of
          Left _ -> return False
          Right c ->
            if c == 0
              then do
                bind_var (m0, 0)
                return True
              else do
                bind_var (m0, recip c)
                return True
    mf _ =
      failWith $
        ErrMsg "internal error in 'encode_zeq'"

-- | Constraint 'y == x==0:1?0'
encode_zeq :: (GaloisField a) => (Var, Var) -> State (CEnv a) ()
encode_zeq (x, y) =
  do
    neg_y <- fresh_var
    encode_zneq (x, neg_y)
    cs_of_exp y (Core.EBinop Sub [Core.EVal 1, Core.EVar (_Var # neg_y)])

-- | Encode the constraint 'un_op x = y'
encode_unop :: (GaloisField a) => UnOp -> (Var, Var) -> State (CEnv a) ()
encode_unop op (x, y) = go op
  where
    go ZEq = encode_zeq (x, y)

-- | Encode the constraint 'x op y = z'.
encode_binop :: (GaloisField a) => Op -> (Var, Var, Var) -> State (CEnv a) ()
encode_binop op (x, y, z) = go op
  where
    go And = encode_binop Mult (x, y, z)
    go Or = encode_or (x, y, z)
    go XOr = encode_xor (x, y, z)
    go Eq = encode_eq (x, y, z)
    go BEq = encode_boolean_eq (x, y, z)
    go Add =
      add_constraint $
        cadd 0 [(x, 1), (y, 1), (z, -1)]
    go Sub =
      add_constraint $
        cadd 0 [(x, 1), (y, -1), (z, -1)]
    go Mult =
      add_constraint $
        CMult (1, x) (1, y) (1, Just z)
    go Div =
      add_constraint $
        CMult (1, y) (1, z) (1, Just x)

encode_linear :: (GaloisField a) => Var -> [Either (Var, a) a] -> State (CEnv a) ()
encode_linear out xs =
  let c = foldl (flip (+)) 0 $ map (fromRight 0) xs
   in add_constraint $
        cadd c $
          (out, -1) : remove_consts xs
  where
    remove_consts :: [Either (Var, a) a] -> [(Var, a)]
    remove_consts [] = []
    remove_consts (Left p : l) = p : remove_consts l
    remove_consts (Right _ : l) = remove_consts l

cs_of_exp :: (GaloisField a) => Var -> Core.Exp a -> State (CEnv a) ()
cs_of_exp out e = case e of
  Core.EVar x ->
    ensure_equal (out, view _Var x)
  Core.EVal c ->
    ensure_const (out, c)
  Core.EUnop op (Core.EVar x) ->
    encode_unop op (view _Var x, out)
  Core.EUnop op e1 ->
    do
      e1_out <- fresh_var
      cs_of_exp e1_out e1
      encode_unop op (e1_out, out)
  Core.EBinop op es ->
    -- [NOTE linear combination optimization:] cf. also
    -- 'encode_linear' above. 'go_linear' returns a list of
    -- (label*coeff + constant) pairs.
    --  (1) The label is the output wire for the expression that was
    -- compiled and the coefficient is its scalar field coefficient,
    -- or 'one' if no coefficient exists (i.e., 'e' is not of the form
    -- 'EBinop Mult [e_left,EVal coeff]' or symmetric.
    --  (2) The constant 'c' is the constant at a particular position
    -- in the list of expressions 'es'.
    -- We special-case linear combinations in this way to avoid having
    -- to introduce new multiplication gates for multiplication by
    -- constant scalars.
    let go_linear :: (GaloisField a) => [Core.Exp a] -> State (CEnv a) [Either (Var, a) a]
        go_linear [] = return []
        go_linear (Core.EBinop Mult [Core.EVar x, Core.EVal coeff] : es') =
          do
            labels <- go_linear es'
            return $ Left (x ^. _Var, coeff) : labels
        go_linear (Core.EBinop Mult [Core.EVal coeff, Core.EVar y] : es') =
          do
            labels <- go_linear es'
            return $ Left (y ^. _Var, coeff) : labels
        go_linear (Core.EBinop Mult [e_left, Core.EVal coeff] : es') =
          do
            e_left_out <- fresh_var
            cs_of_exp e_left_out e_left
            labels <- go_linear es'
            return $ Left (e_left_out, coeff) : labels
        go_linear (Core.EBinop Mult [Core.EVal coeff, e_right] : es') =
          do
            e_right_out <- fresh_var
            cs_of_exp e_right_out e_right
            labels <- go_linear es'
            return $ Left (e_right_out, coeff) : labels
        go_linear (Core.EVal c : es') =
          do
            labels <- go_linear es'
            return $ Right c : labels
        go_linear (Core.EVar x : es') =
          do
            labels <- go_linear es'
            return $ Left (x ^. _Var, 1) : labels

        -- The 'go_linear' catch-all case (i.e., no optimization)
        go_linear (e1 : es') =
          do
            e1_out <- fresh_var
            cs_of_exp e1_out e1
            labels <- go_linear es'
            return $ Left (e1_out, 1) : labels

        go_sub [] = return []
        go_sub (e1 : es') =
          do
            labels <- go_linear (e1 : es')
            case labels of
              [] -> failWith $ ErrMsg "internal error in go_sub"
              k : ls -> return $ k : rev_pol ls

        rev_pol [] = []
        rev_pol (Left (x, c) : ls) = Left (x, -c) : rev_pol ls
        rev_pol (Right c : ls) = Right (-c) : rev_pol ls

        go_other :: (GaloisField a) => [Core.Exp a] -> State (CEnv a) [Var]
        go_other [] = return []
        go_other (Core.EVar x : es') =
          do
            labels <- go_other es'
            return $ (x ^. _Var) : labels
        go_other (e1 : es') =
          do
            e1_out <- fresh_var
            cs_of_exp e1_out e1
            labels <- go_other es'
            return $ e1_out : labels

        encode_labels [] = return ()
        encode_labels [_] = failWith $ ErrMsg ("wrong arity in " ++ show e)
        encode_labels [l1, l2] = encode_binop op (l1, l2, out)
        encode_labels (l1 : l2 : labels') =
          do
            res_out <- fresh_var
            encode_labels (res_out : labels')
            encode_binop op (l1, l2, res_out)
     in do
          case op of
            -- Encode c1x1 + c2x2 + ... + cnxn directly as a linear constraint.
            Add ->
              do
                labels <- go_linear es
                encode_linear out labels
            Sub ->
              do
                labels <- go_sub es
                encode_linear out labels

            -- Otherwise, do the pairwise encoding.
            _ ->
              do
                labels <- go_other es
                encode_labels labels

  -- Encoding: out = b*e1 + (1-b)e2
  Core.EIf b e1 e2 -> cs_of_exp out e0
    where
      e0 =
        Core.EBinop
          Add
          [ Core.EBinop Mult [b, e1],
            Core.EBinop Mult [Core.EBinop Sub [Core.EVal 1, b], e2]
          ]

  ---- NOTE: when compiling assignments, the naive thing to do is
  ---- to introduce a new var, e2_out, bound to result of e2 and
  ---- then ensure that e2_out == x. We optimize by passing x to
  ---- compilation of e2 directly.
  -- EAssert e1 e2 ->
  --  do
  --    let x = var_of_exp e1
  --    cs_of_exp (x ^. _Var) e2
  -- ESeq le ->
  --  do
  --    x <- fresh_var -- x is garbage
  --    go x le
  --  where
  --    go _ [] = failWith $ ErrMsg "internal error: empty ESeq"
  --    go _ [e1] = cs_of_exp out e1
  --    go garbage_var (e1 : le') =
  --      do
  --        cs_of_exp garbage_var e1
  --        go garbage_var le'
  Core.EUnit ->
    -- NOTE: [[ EUnit ]]_{out} = [[ EVal zero ]]_{out}.
    cs_of_exp out (Core.EVal 0)

cs_of_assignment :: (GaloisField a) => Core.Assignment a -> State (CEnv a) ()
cs_of_assignment (Core.Assignment x e) = cs_of_exp (view _Var x) e

data SimplParam
  = NoSimplify
  | Simplify
  | SimplifyDataflow

-- | Snarkl.Compile a list of arithmetic constraints to a rank-1 constraint
-- system.  Takes as input the constraints, the input variables, and
-- the output variables, and return the corresponding R1CS.
compileConstraintsToR1CS ::
  (GaloisField a) =>
  SimplParam ->
  ConstraintSystem a ->
  R1CS a
compileConstraintsToR1CS simpl cs =
  let -- Simplify resulting constraints.
      cs_simpl =
        if must_simplify simpl
          then snd $ do_simplify False Map.empty cs
          else cs
      cs_dataflow = if must_dataflow simpl then removeUnreachable cs_simpl else cs_simpl
      -- Renumber constraint variables sequentially, from 0 to
      -- 'max_var'. 'renumber_f' is a function mapping variables to
      -- their renumbered counterparts.
      (_, cs') = renumber_constraints cs_dataflow
      -- 'f' is a function mapping input bindings to witnesses.
      -- NOTE: we assume the initial variable assignment passed to
      -- 'f' is the one derived by zipping the inputs together with
      -- the (renamed) input vars. of the R1CS produced by this
      -- function. Alternatively, we could 'Map.mapKeys renumber_f'
      -- before applying 'solve cs''.
      f = solve cs'
   in r1cs_of_cs cs' f
  where
    must_simplify :: SimplParam -> Bool
    must_simplify NoSimplify = False
    must_simplify Simplify = True
    must_simplify SimplifyDataflow = True

    must_dataflow :: SimplParam -> Bool
    must_dataflow NoSimplify = False
    must_dataflow Simplify = False
    must_dataflow SimplifyDataflow = True

------------------------------------------------------
--
-- 'TExp'
--
------------------------------------------------------

-- | The result of desugaring a Snarkl computation.
data TExpPkg ty k = TExpPkg
  { -- | The number of free variables in the computation.
    out_variable :: Variable,
    -- | The variables marked as inputs.
    comp_input_variables :: [Variable],
    -- | The resulting 'TExp'.
    comp_texp :: TExp ty k
  }
  deriving (Show)

instance (Typeable ty, Pretty k) => Pretty (TExpPkg ty k) where
  pretty (TExpPkg _ _ e) = pretty e

deriving instance (Eq (TExp ty k)) => Eq (TExpPkg ty k)

-- | Desugar a 'Comp'utation to a pair of:
--   the total number of vars,
--   the input vars,
--   the 'TExp'.
compileCompToTexp ::
  Comp ty k ->
  TExpPkg ty k
compileCompToTexp mf =
  case run mf of
    Left err -> failWith err
    Right (e, rho) ->
      let out = Variable (next_variable rho)
          in_vars = sort $ input_vars rho
       in TExpPkg out in_vars e
  where
    run mf0 =
      runState
        mf0
        ( Env
            0
            0
            []
            Map.empty
            Map.empty
        )

-- | Snarkl.Compile 'TExp's to constraint systems. Re-exported from 'Snarkl.Compile.Snarkl.Compile'.
compileTexpToConstraints ::
  (Typeable ty, GaloisField k, Pretty k) =>
  TExpPkg ty k ->
  ConstraintSystem k
compileTexpToConstraints (TExpPkg _out _in_vars te) =
  let out = _out ^. _Var
      in_vars = map (view _Var) _in_vars
      cenv_init = CEnv Set.empty (incVar out)
      (constrs, _) = State.runState go cenv_init
      go = do
        let boolean_in_vars =
              Set.toList $
                Set.fromList in_vars
                  `Set.intersection` Set.fromList (map (view _Var) $ booleanVarsOfTexp te)
            Core.Program assignments e = expOfTExp te
        traverse_ cs_of_assignment assignments
        -- e = do_const_prop e0
        -- Snarkl.Compile 'e' to constraints 'cs', with output wire 'out'.
        cs_of_assignment $ Core.Assignment (_Var # out) e
        -- Add boolean constraints
        mapM_ ensure_boolean boolean_in_vars
        cs <- get_constraints
        let constraint_set = Set.fromList cs
            num_constraint_vars =
              length $ constraint_vars constraint_set
        return $
          ConstraintSystem
            constraint_set
            num_constraint_vars
            in_vars
            [out]
   in constrs

-- | Snarkl.Compile Snarkl computations to constraint systems.
compileCompToConstraints ::
  (Typeable ty, GaloisField k, Pretty k) =>
  Comp ty k ->
  ConstraintSystem k
compileCompToConstraints = compileTexpToConstraints . compileCompToTexp

------------------------------------------------------
--
-- R1CS
--
------------------------------------------------------

-- | Snarkl.Compile 'TExp's to 'R1CS'.
compileTExpToR1CS ::
  (Typeable ty, GaloisField k, Pretty k) =>
  SimplParam ->
  TExpPkg ty k ->
  R1CS k
compileTExpToR1CS simpl = compileConstraintsToR1CS simpl . compileTexpToConstraints

-- | Snarkl.Compile Snarkl computations to 'R1CS'.
compileCompToR1CS ::
  (Typeable ty, GaloisField k, Pretty k) =>
  SimplParam ->
  Comp ty k ->
  R1CS k
compileCompToR1CS simpl = compileConstraintsToR1CS simpl . compileCompToConstraints

--------------------------------------------------------------------------------

_Var :: Iso' Variable Var
_Var = iso (\(Variable v) -> Var v) (\(Var v) -> Variable v)

--------------------------------------------------------------------------------
