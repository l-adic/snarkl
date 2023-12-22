module Test.Snarkl.DataflowSpec where

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import Snarkl.Common (Var)
import Snarkl.Constraint.Constraints
  ( CoeffList (CoeffList),
    Constraint (..),
    ConstraintSystem (..),
  )
import Snarkl.Constraint.Dataflow
  ( removeUnreachable,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

-- Mock state environment for CMagic (as the real one is not provided)
data SEnv a = SEnv

-- Sample Constraints
-- 3 + 4 * var_1 + 3 * var_2 == 0
constraint1 :: Constraint (Prime p)
constraint1 = CAdd (3 :: (Prime p)) (CoeffList [(1, 4 :: (Prime p)), (2, 3 :: (Prime p))])

-- 2 * var_1 + 3 * var_2 == 4 * var_3
constraint2 :: Constraint (Prime p)
constraint2 = CMult (2 :: (Prime p), 1) (3 :: (Prime p), 2) (4 :: (Prime p), Just 3)

-- NOTE: notice 4 doesn't count as a variable here, WHY?
constraint3 :: Constraint (Prime p)
constraint3 = CMagic 4 [2, 3] $ \vars -> do
  let sumVars = sum vars
  return (sumVars > 5)

-- 4 is independent from 1,2,3
constraint4 :: Constraint (Prime p)
constraint4 = CAdd (3 :: (Prime p)) (CoeffList [(4, 4 :: (Prime p)), (5, 3 :: (Prime p))])

-- 5 is independent from 1,2,3 but intersects 4
constraint5 :: Constraint (Prime p)
constraint5 = CAdd (3 :: (Prime p)) (CoeffList [(4, 4 :: (Prime p)), (1, 3 :: (Prime p))])

-- Example ConstraintSystem
exampleConstraintSystem :: ConstraintSystem (Prime p)
exampleConstraintSystem =
  ConstraintSystem
    { cs_constraints = Set.fromList [constraint1, constraint2, constraint3],
      cs_num_vars = 3,
      cs_in_vars = [1, 2],
      cs_out_vars = [3]
    }

-- Expected Result after removeUnreachable is applied
-- (Adjust this based on the expected behavior of removeUnreachable)
expectedResult :: ConstraintSystem (Prime p)
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
        let fullCs = exampleConstraintSystem {cs_constraints = Set.fromList [constraint1, constraint2, constraint3, constraint4, constraint5]}
        removeUnreachable fullCs `shouldBe` fullCs
