{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Snarkl.Toplevel
  ( -- * Interpret Snarkl Computations
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

import Data.Field.Galois (GaloisField, PrimeField)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Typeable (Typeable)
import Snarkl.AST
import Snarkl.Backend.R1CS
import Snarkl.Common (Assgn (Assgn))
import Snarkl.Compile
import Snarkl.Constraint (ConstraintSystem (cs_in_vars, cs_num_vars, cs_out_vars), SimplifiedConstraintSystem (..), solve)
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Interp (interp)
import Text.PrettyPrint.Leijen.Text (Pretty (..), line, (<+>))

----------------------------------------------------
--
-- Snarkl.Toplevel Stuff
--
----------------------------------------------------

-- | Using the executable semantics for the 'TExp' language, execute
-- the computation on the provided inputs, returning the 'k' result.
comp_interp ::
  (Typeable ty, GaloisField k) =>
  Comp ty k ->
  [k] ->
  k
comp_interp mf inputs =
  let TExpPkg _ in_vars e = compileCompToTexp mf
      input_map = Map.fromList $ zip in_vars inputs
   in case interp input_map e of
        Left err -> failWith err
        Right (_, Nothing) -> failWith $ ErrMsg $ show e ++ " evaluated to bot"
        Right (_, Just v) -> v

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
  pretty (Result sat vars constraints result _ _) =
    mconcat $
      intercalate
        [line]
        [ ["sat" <+> ":=" <+> pretty sat],
          ["vars" <+> ":=" <+> pretty vars],
          ["constraints" <+> ":=" <+> pretty constraints],
          ["result" <+> ":=" <+> pretty result]
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
execute :: (Typeable ty, PrimeField k) => [SimplParam] -> Comp ty k -> [k] -> Result k
execute simpl mf inputs =
  let TExpPkg nv in_vars e = compileCompToTexp mf
      (r1cs, constraintSystem) = compileTExpToR1CS simpl (TExpPkg nv in_vars e)
      [out_var] = r1cs_out_vars r1cs
      wit@(Witness {witness_assgn = Assgn m}) = wit_of_cs inputs constraintSystem
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
      out_interp = comp_interp mf inputs
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
wit_of_cs :: (GaloisField k) => [k] -> SimplifiedConstraintSystem k -> Witness k
wit_of_cs inputs (SimplifiedConstraintSystem cs) =
  let in_vars = cs_in_vars cs
   in if length in_vars /= length inputs
        then
          failWith $
            ErrMsg
              ( "expected "
                  ++ show (length in_vars)
                  ++ " input(s)"
                  ++ " but got "
                  ++ show (length inputs)
                  ++ " input(s)"
              )
        else
          let inputAssignments = Assgn . Map.fromList $ zip in_vars inputs
           in Witness
                { witness_assgn = solve cs inputAssignments,
                  witness_in_vars = in_vars,
                  witness_out_vars = cs_out_vars cs,
                  witness_num_vars = cs_num_vars cs
                }
