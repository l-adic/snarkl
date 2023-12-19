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
constraint1 :: Constraint Rational
constraint1 = CAdd (3 :: Rational) (CoeffList [(1, 4 :: Rational), (2, 3 :: Rational)])

-- 2 * var_1 + 3 * var_2 == 4 * var_3
constraint2 :: Constraint Rational
constraint2 = CMult (2 :: Rational, 1) (3 :: Rational, 2) (4 :: Rational, Just 3)

-- NOTE: notice 4 doesn't count as a variable here, WHY?
constraint3 :: Constraint Rational
constraint3 = CMagic 4 [2, 3] $ \vars -> do
  let sumVars = sum vars
  return (sumVars > 5)

-- 4 is independent from 1,2,3
constraint4 :: Constraint Rational
constraint4 = CAdd (3 :: Rational) (CoeffList [(4, 4 :: Rational), (5, 3 :: Rational)])

-- 5 is independent from 1,2,3 but intersects 4
constraint5 :: Constraint Rational
constraint5 = CAdd (3 :: Rational) (CoeffList [(4, 4 :: Rational), (1, 3 :: Rational)])

-- Example ConstraintSystem
exampleConstraintSystem :: ConstraintSystem Rational
exampleConstraintSystem =
  ConstraintSystem
    { cs_constraints = Set.fromList [constraint1, constraint2, constraint3],
      cs_num_vars = 3,
      cs_in_vars = [1, 2],
      cs_out_vars = [3]
    }

-- Expected Result after removeUnreachable is applied
-- (Adjust this based on the expected behavior of removeUnreachable)
expectedResult :: ConstraintSystem Rational
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