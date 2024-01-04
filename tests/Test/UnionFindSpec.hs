{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Use uncurry" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test.UnionFindSpec where

import Snarkl.Common
import Snarkl.Constraint.UnionFind
import Snarkl.Errors
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "UnionFind Algorithm" $ do
    it "finds the root of a single element correctly" $
      property $
        \x -> root (unite empty (Var x) (Var x) :: UnionFind Var Int) (Var x) `shouldBe` (Var x, unite empty (Var x) (Var x))

    it "unifies two elements correctly" $
      property $
        forAll (arbitrary `suchThat` \(x, y) -> x < y) $ \(x, y) ->
          let uf :: UnionFind Var Int
              uf = unite empty (Var x) (Var y)
              rx = fst $ root uf (Var x)
              ry = fst $ root uf (Var y)
           in rx `shouldBe` ry

    it "unifies 3 elements correctly" $
      property $
        forAll (arbitrary `suchThat` \(x, y, z) -> x < y && y < z) $ \(x, y, z) ->
          let uf :: UnionFind Var String
              uf = insert (Var z) "X" (insert (Var y) "X" (insert (Var x) "X" empty))
              uf' = unite uf (Var x) (Var y)
           in do
                fst (root uf' (Var x)) `shouldBe` Var x
                fst (root uf' (Var y)) `shouldBe` Var x
                fst (root uf' (Var z)) `shouldBe` Var z
                let uf'' = unite uf' (Var y) (Var z)
                fst (root uf'' (Var z)) `shouldBe` Var x
