{-# LANGUAGE ScopedTypeVariables #-}

module Test.Snarkl.DataflowSpec where

import Data.Field.Galois (GaloisField, Prime, PrimeField, toP)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.TypeLits (KnownNat)
import Snarkl.Common (Var (Var))
import Snarkl.Constraint
  ( CoeffList (CoeffList),
    Constraint (..),
    ConstraintSystem (..),
    removeUnreachable,
  )
import Snarkl.Field (F_BN128, P_BN128)
import Test.Hspec (Spec, describe, it, shouldBe)

-- Mock state environment for CMagic (as the real one is not provided)
data SEnv a = SEnv

-- Sample Constraints
-- 3 + 4 * var_1 + 3 * var_2 == 0
constraint1 :: (KnownNat p) => Constraint (Prime p)
constraint1 = CAdd (toP 3) (CoeffList [(Var 1, toP 4), (Var 2, toP 3)])

-- 2 * var_1 + 3 * var_2 == 4 * var_3
constraint2 :: (KnownNat p) => Constraint (Prime p)
constraint2 = CMult (toP 2, Var 1) (toP 3, Var 2) (toP 4, Just $ Var 3)

-- NOTE: notice 4 doesn't count as a variable here, WHY?
constraint3 :: (GaloisField k) => Constraint k
constraint3 = CMagic (Var 4) [Var 2, Var 3] $ \vars -> return True

-- 4 is independent from 1,2,3
constraint4 :: (KnownNat p) => Constraint (Prime p)
constraint4 = CAdd (toP 3) (CoeffList [(Var 4, toP 4), (Var 5, toP 3)])

-- 5 is independent from 1,2,3 but intersects 4
constraint5 :: (KnownNat p) => Constraint (Prime p)
constraint5 = CAdd (toP 3) (CoeffList [(Var 4, toP 4), (Var 1, toP 3)])

-- Example ConstraintSystem
exampleConstraintSystem :: ConstraintSystem (Prime P_BN128)
exampleConstraintSystem =
  ConstraintSystem
    { cs_constraints = Set.fromList [constraint1, constraint2, constraint3],
      cs_num_vars = 3,
      cs_public_in_vars = [Var 1, Var 2],
      cs_out_vars = [Var 3]
    }

-- Expected Result after removeUnreachable is applied
-- (Adjust this based on the expected behavior of removeUnreachable)
expectedResult :: ConstraintSystem F_BN128
expectedResult =
  exampleConstraintSystem
    { cs_constraints = Set.fromList [constraint1, constraint2, constraint3, constraint5]
    } -- Replace with expected result

spec :: Spec
spec = do
  describe "Snarkl.Constraint.Dataflow" $ do
    describe "removeUnreachable" $ do
      it "doesn't do anything if all variables are reached from output variables" $ do
        removeUnreachable exampleConstraintSystem
          `shouldBe` exampleConstraintSystem

      it "trims variables unreaded from output variables " $
        removeUnreachable (exampleConstraintSystem {cs_constraints = Set.fromList [constraint1, constraint2, constraint3, constraint4]})
          `shouldBe` exampleConstraintSystem

      it "includes all constraints as long as the intersect output vars " $ do
        let fullCs = exampleConstraintSystem {cs_constraints = Set.fromList [constraint1 :: Constraint (Prime P_BN128), constraint2, constraint3, constraint4, constraint5]}
        removeUnreachable fullCs `shouldBe` fullCs
