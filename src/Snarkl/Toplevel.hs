{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Snarkl.Toplevel
  ( PublicInputs (..),
    PrivateInputs (..),

    -- * Interpret Snarkl Computations
    comp_interp,

    -- * Convenience functions
    Result (..),
    execute,
    wit_of_cs,

    -- * Re-exported modules
    module Snarkl.AST,
    module Snarkl.Backend.R1CS,
    module Snarkl.Compile,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (Bifunctor (second))
import Data.Field.Galois (GaloisField, PrimeField)
import Data.JSONLines (FromJSONLines (fromJSONLines), NoHeader (..), ToJSONLines (toJSONLines))
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Snarkl.AST
import Snarkl.Backend.R1CS
import Snarkl.Common (Assgn (Assgn), FieldElem (..), Var)
import Snarkl.Compile
import Snarkl.Constraint (ConstraintSystem (cs_num_vars, cs_out_vars, cs_public_in_vars), SimplifiedConstraintSystem (..), solve)
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Interp (interp)
import Text.PrettyPrint.Leijen.Text (Pretty (..), indent, line, (<+>))

----------------------------------------------------
--
-- Snarkl.Toplevel Stuff
--
----------------------------------------------------

newtype PublicInputs k = PublicInputs {unPublicInputs :: [k]}
  deriving (Show)

newtype PrivateInputs k = PrivateInputs {unPrivateInputs :: Map.Map String k}
  deriving (Show)

instance (PrimeField k) => ToJSONLines (PrivateInputs k) where
  toJSONLines (PrivateInputs m) = toJSONLines $ NoHeader (Map.toList $ FieldElem <$> m)

instance (PrimeField k) => FromJSONLines (PrivateInputs k) where
  fromJSONLines ls = do
    NoHeader kvs <- fromJSONLines ls
    pure . PrivateInputs . Map.fromList $ map (second unFieldElem) kvs

-- | Using the executable semantics for the 'TExp' language, execute
-- the computation on the provided inputs, returning the 'k' result.
comp_interp ::
  (Typeable ty, GaloisField k) =>
  Comp ty k ->
  [k] ->
  Map.Map String k ->
  k
comp_interp mf inputs privateInputs =
  let texpPkg@(TExpPkg _ public_in_vars private_in_vars e) = compileCompToTexp mf
   in case sanatizeInput texpPkg of
        Left err -> failWith $ ErrMsg err
        Right () ->
          let privateAssignments =
                [ (var, val)
                  | (k, val) <- Map.toList privateInputs,
                    (k', var) <- Map.toList private_in_vars,
                    k == k'
                ]
              inputAssignments = Map.fromList $ privateAssignments <> zip public_in_vars inputs
           in case interp inputAssignments e of
                Left err -> failWith err
                Right (_, Nothing) -> failWith $ ErrMsg $ show e ++ " evaluated to bot"
                Right (_, Just v) -> v
  where
    sanatizeInput (TExpPkg _ pubIn privIn _) = do
      let unassignedPrivateVars = Map.keysSet privIn `Set.difference` Map.keysSet privateInputs
      unless (Set.null unassignedPrivateVars) $
        throwError $
          "private inputs missing for variables: " <> show (Set.toList unassignedPrivateVars)
      unless (length pubIn == length inputs) $
        throwError $
          "expected " <> show (length pubIn) <> " input(s) but got " <> show (length inputs) <> " input(s)"

-- | The result of compiling and executing a Snarkl computation.
data Result k = Result
  { result_sat :: Bool,
    result_vars :: Int,
    result_constraints :: Int,
    result_result :: k,
    result_r1cs :: R1CS k,
    result_witness :: Witness k
  }
  deriving (Show)

instance (Pretty k) => Pretty (Result k) where
  pretty (Result sat vars constraints result r1cs witness) =
    mconcat $
      intercalate
        [line]
        [ ["system is satisfied" <+> "=" <+> pretty sat],
          ["number of vars" <+> "=" <+> pretty vars],
          ["number of constraints" <+> "=" <+> pretty constraints],
          ["result" <+> "=" <+> pretty result],
          ["r1cs" <+> ":" <> line <> indent 4 (pretty r1cs)],
          ["witness" <+> ":" <> line <> indent 4 (pretty witness)]
        ]

--------------------------------------------------
--
-- Internal Functions
--
--------------------------------------------------

-- | (1) Snarkl.Compile to R1CS.
--   (2) Generate a satisfying assignment, 'w'.
--   (3) Check whether 'w' satisfies the constraint system produced in (1).
--   (4) Check whether the R1CS result matches the interpreter result.
--   (5) Return the 'Result'.
execute ::
  (Typeable ty) =>
  (PrimeField k) =>
  [SimplParam] ->
  Comp ty k ->
  [k] ->
  Map.Map String k ->
  Result k
execute simpl mf inputs privateInputs =
  let texpPkg = compileCompToTexp mf
      (r1cs, constraintSystem, privateInputVars) = compileTExpToR1CS simpl texpPkg
      privateAssignments =
        Map.fromList $
          [ (var, val)
            | (k, val) <- Map.toList privateInputs,
              (k', var) <- Map.toList privateInputVars,
              k == k'
          ]
      [out_var] = r1cs_out_vars r1cs
      wit@(Witness {witness_assgn = Assgn m}) = wit_of_cs inputs privateAssignments constraintSystem
      out = case Map.lookup out_var m of
        Nothing ->
          failWith $
            ErrMsg
              ( "output variable "
                  ++ show out_var
                  ++ "not mapped, in\n  "
                  ++ show wit
              )
        Just out_val -> out_val
      -- Interpret the program using the executable semantics and
      -- the input assignment (a subset of 'wit').
      -- Output the return value of 'e'.
      out_interp = comp_interp mf inputs privateInputs
      nw = r1cs_num_vars r1cs
      ng = num_constraints r1cs
      result = out_interp == out && sat_r1cs wit r1cs
   in if result
        then Result result nw ng out r1cs wit
        else
          failWith $
            ErrMsg $
              "interpreter result "
                ++ show out_interp
                ++ " differs from actual result "
                ++ show out

-- | For a given R1CS and inputs, calculate a satisfying assignment.
wit_of_cs ::
  (GaloisField k) =>
  [k] ->
  Map.Map Var k ->
  SimplifiedConstraintSystem k ->
  Witness k
wit_of_cs inputs privateInputs (SimplifiedConstraintSystem cs) =
  let public_in_vars = cs_public_in_vars cs
   in if length public_in_vars /= length inputs
        then
          failWith $
            ErrMsg
              ( "expected "
                  ++ show (length public_in_vars)
                  ++ " input(s)"
                  ++ " but got "
                  ++ show (length inputs)
                  ++ " input(s)"
              )
        else
          let inputAssignments = Assgn $ privateInputs `Map.union` Map.fromList (zip public_in_vars inputs)
           in Witness
                { witness_assgn = solve cs inputAssignments,
                  witness_in_vars = public_in_vars,
                  witness_out_vars = cs_out_vars cs,
                  witness_num_vars = cs_num_vars cs
                }
