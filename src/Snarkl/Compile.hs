{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Snarkl.Compile
  ( TExpPkg (..),
    SimplParam (..),
    compileTExpToR1CS,
    compileCompToR1CS,
    compileCompToTexp,
    compileTexpToConstraints,
  )
where

import Control.Arrow (Arrow (second))
import Control.Lens (Iso', iso, view, (#), (^.))
import Control.Monad.State
  ( State,
    gets,
    modify,
  )
import qualified Control.Monad.State as State
import Data.Either (fromRight)
import Data.Field.Galois (GaloisField)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Sequence (pattern Empty, pattern (:<|))
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Snarkl.AST
  ( Comp,
    Env (..),
    Exp (..),
    InputVariable (..),
    TExp,
    Variable (Variable),
    booleanVarsOfTexp,
    defaultEnv,
    do_const_prop,
    expOfTExp,
    runComp,
    var_of_exp,
  )
import Snarkl.Backend.R1CS.R1CS (R1CS)
import Snarkl.Common (Assgn (Assgn), Op (..), UnOp (..), Var (Var), incVar)
import Snarkl.Constraint
  ( Constraint (CMagic, CMult),
    ConstraintSystem (ConstraintSystem),
    SimplifiedConstraintSystem (..),
    bind_of_var,
    bind_var,
    cadd,
    constraint_vars,
    do_simplify,
    r1cs_of_cs,
    removeUnreachable,
    renumber_constraints,
  )
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Text.PrettyPrint.Leijen.Text (Pretty (..))

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
    cs_of_exp x_mult_y (EBinop Mult [EVar (_Var # x), EVar (_Var # y)])
    cs_of_exp
      x_mult_y
      ( EBinop
          Sub
          [ EBinop Add [EVar (_Var # x), EVar (_Var # y)],
            EVar (_Var # z)
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
      EBinop
        Add
        [ EBinop Mult [EVar (_Var # x), EVar (_Var # y)],
          EBinop
            Mult
            [ EBinop Sub [EVal 1, EVar (_Var # x)],
              EBinop Sub [EVal 1, EVar (_Var # y)]
            ]
        ]

-- | Constraint 'x == y = z'.
-- The encoding is: z = (x-y == 0).
encode_eq :: (GaloisField a) => (Var, Var, Var) -> State (CEnv a) ()
encode_eq (x, y, z) = cs_of_exp z e
  where
    e =
      EAssert
        (EVar (_Var # z))
        (EUnop ZEq (EBinop Sub [EVar (_Var # x), EVar (_Var # y)]))

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
    cs_of_exp y (EBinop Mult [EVar (_Var # x), EVar (_Var # m)])
    cs_of_exp neg_y (EBinop Sub [EVal 1, EVar (_Var # y)])
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
    cs_of_exp y (EBinop Sub [EVal 1, EVar (_Var # neg_y)])

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

cs_of_exp :: (GaloisField a) => Var -> Exp a -> State (CEnv a) ()
cs_of_exp out e = case e of
  EVar x -> do
    ensure_equal (out, view _Var x)
  EVal c -> do
    ensure_const (out, c)
  EUnop op (EVar x) -> do
    encode_unop op (view _Var x, out)
  EUnop op e1 ->
    do
      e1_out <- fresh_var
      cs_of_exp e1_out e1
      encode_unop op (e1_out, out)
  EBinop op es ->
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
    let go_linear :: (GaloisField a) => [Exp a] -> State (CEnv a) [Either (Var, a) a]
        go_linear [] = return []
        go_linear (EBinop Mult [EVar x, EVal coeff] : es') =
          do
            labels <- go_linear es'
            return $ Left (x ^. _Var, coeff) : labels
        go_linear (EBinop Mult [EVal coeff, EVar y] : es') =
          do
            labels <- go_linear es'
            return $ Left (y ^. _Var, coeff) : labels
        go_linear (EBinop Mult [e_left, EVal coeff] : es') =
          do
            e_left_out <- fresh_var
            cs_of_exp e_left_out e_left
            labels <- go_linear es'
            return $ Left (e_left_out, coeff) : labels
        go_linear (EBinop Mult [EVal coeff, e_right] : es') =
          do
            e_right_out <- fresh_var
            cs_of_exp e_right_out e_right
            labels <- go_linear es'
            return $ Left (e_right_out, coeff) : labels
        go_linear (EVal c : es') =
          do
            labels <- go_linear es'
            return $ Right c : labels
        go_linear (EVar x : es') =
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

        go_other :: (GaloisField a) => [Exp a] -> State (CEnv a) [Var]
        go_other [] = return []
        go_other (EVar x : es') =
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
  EIf b e1 e2 -> do
    cs_of_exp out e0
    where
      e0 =
        EBinop
          Add
          [ EBinop Mult [b, e1],
            EBinop Mult [EBinop Sub [EVal 1, b], e2]
          ]

  -- NOTE: when compiling assignments, the naive thing to do is
  -- to introduce a new var, e2_out, bound to result of e2 and
  -- then ensure that e2_out == x. We optimize by passing x to
  -- compilation of e2 directly.
  EAssert e1 e2 ->
    do
      let x = var_of_exp e1
      cs_of_exp (x ^. _Var) e2
  ESeq le ->
    do
      x <- fresh_var -- x is garbage
      go x le
    where
      go _ Empty = failWith $ ErrMsg "internal error: empty ESeq"
      go _ (e1 :<| Empty) = cs_of_exp out e1
      go garbage_var (e1 :<| le') =
        do
          cs_of_exp garbage_var e1
          go garbage_var le'
  EUnit -> do
    -- NOTE: [[ EUnit ]]_{out} = [[ EVal zero ]]_{out}.
    cs_of_exp out (EVal 0)

data SimplParam
  = NoSimplify
  | Simplify
  | RemoveUnreachable
  deriving (Eq)

-- | Snarkl.Compile a list of arithmetic constraints to a rank-1 constraint
-- system.  Takes as input the constraints, the input variables, and
-- the output variables, and return the corresponding R1CS.
compileConstraintsToR1CS ::
  (GaloisField a) =>
  [SimplParam] ->
  ConstraintSystem a ->
  (R1CS a, SimplifiedConstraintSystem a, Var -> Var)
compileConstraintsToR1CS simpls cs =
  let -- Simplify resulting constraints.
      cs_simpl =
        if must_simplify
          then snd $ do_simplify False (Assgn Map.empty) cs
          else cs
      cs_dataflow = if must_dataflow then removeUnreachable cs_simpl else cs_simpl
      -- Renumber constraint variables sequentially, from 0 to
      -- 'max_var'. 'renumber_f' is a function mapping variables to
      -- their renumbered counterparts.
      (relabler, simplifiedCS) = second SimplifiedConstraintSystem $ renumber_constraints cs_dataflow
   in -- 'f' is a function mapping input bindings to witnesses.
      -- NOTE: we assume the initial variable assignment passed to
      -- 'f' is the one derived by zipping the inputs together with
      -- the (renamed) input vars. of the R1CS produced by this
      -- function. Alternatively, we could 'Map.mapKeys renumber_f'
      -- before applying 'solve cs''.
      (r1cs_of_cs simplifiedCS, simplifiedCS, relabler)
  where
    must_simplify :: Bool
    must_simplify = Simplify `elem` simpls

    must_dataflow :: Bool
    must_dataflow = RemoveUnreachable `elem` simpls

------------------------------------------------------
--
-- 'TExp'
--
------------------------------------------------------

-- | The result of desugaring a Snarkl computation.
data TExpPkg ty k = TExpPkg
  { -- | The number of free variables in the computation.
    out_variable :: Variable,
    -- | The variables marked as public inputs.
    comp_public_input_variables :: [Variable],
    -- | The variables marked as private inputs.
    comp_private_input_variables :: Map.Map String Variable,
    -- | The resulting 'TExp'.
    comp_texp :: TExp ty k
  }
  deriving (Show)

deriving instance (Eq k) => Eq (TExpPkg ty k)

instance (Pretty k, Typeable ty) => Pretty (TExpPkg ty k) where
  pretty (TExpPkg _ _ _ e) =
    pretty e

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
      let (public, private) =
            foldl
              ( \(pub, priv) var ->
                  case var of
                    PublicInput v -> (v : pub, priv)
                    PrivateInput name v -> (pub, Map.insert name v priv)
              )
              ([], Map.empty)
              (input_vars rho)
          out = Variable (next_variable rho)
          in_vars = sort public
       in TExpPkg out in_vars private e
  where
    run mf0 = runComp mf0 defaultEnv

-- | Snarkl.Compile 'TExp's to constraint systems. Re-exported from 'Snarkl.Compile.Snarkl.Compile'.
compileTexpToConstraints ::
  (Typeable ty, GaloisField k) =>
  TExpPkg ty k ->
  ConstraintSystem k
compileTexpToConstraints (TExpPkg _out _public_in_vars _private_in_vars te) =
  let out = _out ^. _Var
      public_in_vars = map (view _Var) _public_in_vars
      private_in_vars = fmap (view _Var) _private_in_vars
      cenv_init = CEnv Set.empty (incVar out)
      (constrs, _) = State.runState go cenv_init
      go = do
        let boolean_in_vars =
              Set.toList $
                Set.fromList (public_in_vars <> Map.elems private_in_vars)
                  `Set.intersection` Set.fromList (map (view _Var) $ booleanVarsOfTexp te)
            e0 = expOfTExp te
            e = do_const_prop e0
        -- Snarkl.Compile 'e' to constraints 'cs', with output wire 'out'.
        cs_of_exp out e
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
            public_in_vars
            [out]
   in constrs

------------------------------------------------------
--
-- R1CS
--
------------------------------------------------------

-- | Snarkl.Compile 'TExp's to 'R1CS'.
compileTExpToR1CS ::
  (Typeable ty, GaloisField k) =>
  [SimplParam] ->
  TExpPkg ty k ->
  (R1CS k, SimplifiedConstraintSystem k, Map.Map String Var)
compileTExpToR1CS simpl texpPkg =
  let (r1cs, scs, relabler) = compileConstraintsToR1CS simpl . compileTexpToConstraints $ texpPkg
   in (r1cs, scs, relabler . view _Var <$> comp_private_input_variables texpPkg)

-- | Snarkl.Compile Snarkl computations to 'R1CS'.
compileCompToR1CS ::
  (Typeable ty, GaloisField k) =>
  [SimplParam] ->
  Comp ty k ->
  (R1CS k, SimplifiedConstraintSystem k, Map.Map String Var)
compileCompToR1CS simpl comp =
  let texpPkg = compileCompToTexp comp
      (r1cs, scs, relabler) = compileConstraintsToR1CS simpl . compileTexpToConstraints $ texpPkg
   in (r1cs, scs, relabler . view _Var <$> comp_private_input_variables texpPkg)

--------------------------------------------------------------------------------

_Var :: Iso' Variable Var
_Var = iso (\(Variable v) -> Var v) (\(Var v) -> Variable v)
