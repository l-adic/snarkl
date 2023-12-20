{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Use uncurry" #-}
{-# LANGUAGE NamedFieldPuns #-}
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
        \x -> root (unite empty x x :: UnionFind Var Int) x `shouldBe` (x, unite empty x x)

    it "unifies two elements correctly" $
      property $
        forAll (arbitrary `suchThat` \(x, y) -> x < y) $ \(x, y) ->
          let uf :: UnionFind Var Int
              uf = unite empty x y
              rx = fst $ root uf x
              ry = fst $ root uf y
           in rx `shouldBe` ry

    it "unifies 3 elements correctly" $
      property $
        forAll (arbitrary `suchThat` \(x, y, z) -> x < y && y < z) $ \(x, y, z) ->
          let uf :: UnionFind Var String
              uf = insert z "X" (insert y "X" (insert x "X" empty))
              uf' = unite uf x y
           in do
                fst (root uf' x) `shouldBe` x
                fst (root uf' y) `shouldBe` x
                fst (root uf' z) `shouldBe` z
                let uf'' = unite uf' y z
                fst (root uf'' z) `shouldBe` x
