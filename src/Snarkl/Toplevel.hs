{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Snarkl.Toplevel
  ( -- * Interpret Snarkl Computations
    comp_interp,

    -- * Convenience functions
    Result (..),
    execute,
    execute',
    r1csOnly,

    -- * Re-exported modules
    module Snarkl.Backend.R1CS,
    module Snarkl.Compile,
  )
where

import Data.Field.Galois (GaloisField, PrimeField)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Snarkl.Backend.R1CS
import Snarkl.Common (Assgn)
import Snarkl.Compile
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Interp (interp)
import Snarkl.Language
import Text.PrettyPrint.Leijen.Text (Pretty (..), line, (<+>))
import Prelude

-- | Using the executable semantics for the 'TExp' language, execute
-- the computation on the provided inputs, returning the 'k' result.
comp_interp ::
  forall ty k.
  (GaloisField k) =>
  Comp ty k ->
  [k] ->
  k
comp_interp mf inputs =
  comp_interp' mf inputs Map.empty

comp_interp' ::
  forall ty k.
  (GaloisField k) =>
  Comp ty k ->
  [k] ->
  Map.Map String k ->
  k
comp_interp' mf inputs knownAssingments =
  let TExpPkg _ in_vars _known e = compileCompToTexp mf
      knownVars =
        [ (var, val)
          | (k, val) <- Map.toList knownAssingments,
            (k', var) <- Map.toList _known,
            k == k'
        ]
      input_map = Map.fromList $ knownVars <> zip in_vars inputs
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
    result_witness :: Assgn k
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
execute ::
  (Typeable ty) =>
  (PrimeField k) =>
  SimplParam ->
  Comp ty k ->
  [k] ->
  Result k
execute simpl mf inputs =
  execute' simpl mf inputs Map.empty

execute' ::
  (Typeable ty) =>
  (PrimeField k) =>
  SimplParam ->
  Comp ty k ->
  [k] ->
  Map.Map String k ->
  Result k
execute' simpl mf inputs knownVars =
  let TExpPkg nv in_vars _known e = compileCompToTexp mf
   in if Set.null $ Set.fromList (Map.keys _known) Set.\\ Set.fromList (Map.keys knownVars)
        then
          let p = TExpPkg nv in_vars _known e
              r1cs = compileTExpToR1CS simpl p
              [out_var] = r1cs_out_vars r1cs
              wit = wit_of_r1cs inputs knownVars r1cs
              out = case Map.lookup out_var wit of
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
              out_interp = comp_interp' mf inputs knownVars
              -- in error $ show (out_interp, length (r1cs_clauses r1cs), r1cs_num_vars r1cs)

              result =
                ( if out_interp == out
                    then sat_r1cs wit r1cs
                    else
                      failWith $
                        ErrMsg $
                          "interpreter result "
                            ++ show out_interp
                            ++ " differs from actual result "
                            ++ show out
                )
              nw = r1cs_num_vars r1cs
              ng = num_constraints r1cs
           in Result result nw ng out r1cs wit
        else
          failWith $
            ErrMsg $
              "knownVars "
                ++ show (Map.keys knownVars)
                ++ " does not match expected knownVars "
                ++ show (Map.keys _known)

r1csOnly ::
  (Typeable ty) =>
  (PrimeField k) =>
  SimplParam ->
  Comp ty k ->
  R1CS k
r1csOnly simpl mf =
  let p = compileCompToTexp mf
   in compileTExpToR1CS simpl p