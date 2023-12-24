{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant curry" #-}
{-# HLINT ignore "Redundant uncurry" #-}
module Test.Snarkl.LambdaSpec where

import Data.Field.Galois (GaloisField, Prime)
import qualified Data.Map as Map
import GHC.TypeLits (KnownNat)
import Snarkl.Field
import Snarkl.Interp (interp)
import Snarkl.Language.Syntax
  ( apply,
    curry,
    lambda,
    pair,
    uncurry,
    (*),
    (+),
  )
import qualified Snarkl.Language.SyntaxMonad as SM
import Snarkl.Language.TExpr (TExp, Ty (TField, TFun, TProd))
import Snarkl.Toplevel (comp_interp)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Testable (property))
import Prelude hiding (apply, curry, return, uncurry, (*), (+))

spec :: Spec
spec = do
  describe "Snarkl.Lambda" $ do
    describe "curry/uncurry identities for simply operations" $ do
      it "curry . uncurry == id" $ do
        let f :: (GaloisField k) => TExp 'TField k -> SM.Comp ('TFun 'TField 'TField) k
            f x = lambda $ \y -> SM.return (x + y)
            g :: (GaloisField k) => TExp 'TField k -> SM.Comp ('TFun 'TField 'TField) k
            g = curry (uncurry f)
            prog1 :: (GaloisField k) => SM.Comp 'TField k
            prog1 =
              SM.fresh_input SM.>>= \a ->
                SM.fresh_input SM.>>= \b ->
                  f a SM.>>= \k ->
                    apply k b
            prog2 :: (GaloisField k) => SM.Comp 'TField k
            prog2 =
              SM.fresh_input SM.>>= \a ->
                SM.fresh_input SM.>>= \b ->
                  g a SM.>>= \k ->
                    apply k b
        property $ \a b ->
          comp_interp @_ @F_BN128 prog1 [a, b] == comp_interp prog2 [a, b]

      it "uncurry . curry == id" $ do
        let f :: (GaloisField k) => TExp ('TProd 'TField 'TField) k -> SM.Comp 'TField k
            f p =
              SM.fst_pair p SM.>>= \x ->
                SM.snd_pair p SM.>>= \y ->
                  SM.return (x * y)
            g :: (GaloisField k) => TExp ('TProd 'TField 'TField) k -> SM.Comp 'TField k
            g = uncurry (curry f)

            prog1 :: (GaloisField k) => SM.Comp 'TField k
            prog1 =
              SM.fresh_input SM.>>= \a ->
                SM.fresh_input SM.>>= \b ->
                  pair a b SM.>>= \p ->
                    f p
            prog2 :: (GaloisField k) => SM.Comp 'TField k
            prog2 =
              SM.fresh_input SM.>>= \a ->
                SM.fresh_input SM.>>= \b ->
                  pair a b SM.>>= \p ->
                    g p
        property $ \a b -> comp_interp @_ @F_BN128 prog1 [a, b] == comp_interp prog2 [a, b]
