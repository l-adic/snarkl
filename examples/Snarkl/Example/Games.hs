{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snarkl.Example.Games where

import Data.Field.Galois (GaloisField, Prime)
import qualified Data.Map as Map
import Data.Typeable
import GHC.TypeLits (KnownNat, Nat)
import Snarkl.Errors
import Snarkl.Field (F_BN128)
import Snarkl.Language.Prelude
import Snarkl.Toplevel (comp_interp)
import Prelude hiding
  ( fromRational,
    negate,
    return,
    (&&),
    (*),
    (+),
    (-),
    (/),
    (>>),
    (>>=),
  )

{---------------------------------------------------------
  See Vytiniotis & Kennedy,
  "Functional Pearl: Every Bit Counts", ICFP 2010
 ---------------------------------------------------------}

data ISO (t :: Ty) (s :: Ty) k = Iso
  { to :: TExp t k -> Comp s k,
    from :: TExp s k -> Comp t k
  }

data Game :: Ty -> * -> * where
  Single ::
    forall (s :: Ty) (t :: Ty) k.
    ( Typeable s,
      Typeable t
    ) =>
    ISO t s k ->
    Game t k
  Split ::
    forall (t1 :: Ty) (t2 :: Ty) (t :: Ty) k.
    ( Typeable t1,
      Typeable t2,
      Typeable t,
      Zippable t1 k,
      Zippable t2 k,
      Zippable t k,
      Derive t1 k,
      Derive t2 k
    ) =>
    ISO t ('TSum t1 t2) k ->
    Game t1 k ->
    Game t2 k ->
    Game t k

decode :: (GaloisField k) => Game t k -> Comp t k
decode (Single (Iso _ bld)) =
  do
    x <- fresh_public_input
    bld x
decode (Split (Iso _ bld) g1 g2) =
  do
    x <- fresh_public_input
    e1 <- decode g1
    e2 <- decode g2
    s1 <- inl e1
    s2 <- inr e2
    v1 <- bld s1
    v2 <- bld s2
    if return x then return v2 else return v1

field_game :: Game 'TField p
field_game = Single (Iso return return)

bool_game :: (GaloisField k) => Game 'TBool k
bool_game =
  Single
    ( Iso
        (\be -> if return be then return (fromField 1) else return (fromField 0))
        (\te -> if return (zeq te) then return false else return true)
    )

unit_game :: (GaloisField k) => Game 'TUnit k
unit_game = Single (Iso (\_ -> return (fromField 1)) (\(_ :: TExp 'TField k) -> return unit))

fail_game :: (Typeable ty) => Game ty p
fail_game =
  Single
    ( Iso
        (\_ -> failWith $ ErrMsg "fail-games can't encode")
        ( \(_ :: TExp 'TField k) ->
            failWith $ ErrMsg "fail-games can't decode"
        )
    )

sum_game ::
  ( Typeable t1,
    Typeable t2,
    Zippable t1 k,
    Zippable t2 k,
    Derive t1 k,
    Derive t2 k,
    GaloisField k
  ) =>
  Game t1 k ->
  Game t2 k ->
  Game ('TSum t1 t2) k
sum_game g1 g2 =
  Split (Iso return return) g1 g2

basic_game :: (GaloisField k) => Game ('TSum 'TField 'TField) k
basic_game = sum_game field_game field_game

basic_test :: (GaloisField k) => Comp 'TField k
basic_test =
  do
    s <- decode basic_game
    case_sum return return s

t1 :: F_BN128
t1 = comp_interp basic_test [0, 23, 88] Map.empty -- 23

t2 :: F_BN128
t2 = comp_interp basic_test [1, 23, 88] Map.empty -- 88

(+>) ::
  ( Typeable t,
    Typeable s,
    Zippable t k,
    Zippable s k
  ) =>
  Game t k ->
  ISO s t k ->
  Game s k
(Single j) +> i = Single (i `seqI` j)
(Split j g1 g2) +> i = Split (i `seqI` j) g1 g2

idI :: ISO a a p
idI = Iso return return

prodI ::
  ( Typeable a,
    Typeable b,
    Typeable c,
    Typeable d,
    GaloisField k
  ) =>
  ISO a b k ->
  ISO c d k ->
  ISO ('TProd a c) ('TProd b d) k
prodI (Iso f g) (Iso f' g') =
  Iso
    ( \p -> do
        x1 <- fst_pair p
        x2 <- snd_pair p
        y1 <- f x1
        y2 <- f' x2
        pair y1 y2
    )
    ( \p -> do
        x1 <- fst_pair p
        x2 <- snd_pair p
        y1 <- g x1
        y2 <- g' x2
        pair y1 y2
    )

seqI :: (Typeable b) => ISO a b p -> ISO b c p -> ISO a c p
seqI (Iso f g) (Iso f' g') = Iso (\a -> f a >>= f') (\c -> g' c >>= g)

prodLInputI ::
  ( Typeable a,
    Typeable b,
    GaloisField k
  ) =>
  ISO ('TProd a b) b k
prodLInputI =
  Iso
    snd_pair
    ( \b -> do
        a <- fresh_public_input
        pair a b
    )

prodLSumI ::
  ( Typeable a,
    Typeable b,
    Typeable c,
    Zippable a k,
    Zippable b k,
    Zippable c k,
    Derive a k,
    Derive b k,
    Derive c k,
    GaloisField k
  ) =>
  ISO ('TProd ('TSum b c) a) ('TSum ('TProd b a) ('TProd c a)) k
prodLSumI =
  Iso
    ( \p -> do
        xbc <- fst_pair p
        xa <- snd_pair p
        case_sum
          ( \xb -> do
              p' <- pair xb xa
              inl p'
          )
          ( \xc -> do
              p' <- pair xc xa
              inr p'
          )
          xbc
    )
    ( \s -> do
        case_sum
          ( \pba -> do
              a <- snd_pair pba
              b <- fst_pair pba
              sb <- inl b
              pair sb a
          )
          ( \pca -> do
              a <- snd_pair pca
              c <- fst_pair pca
              sc <- inr c
              pair sc a
          )
          s
    )

prod_game ::
  ( Typeable b,
    Zippable a k,
    Zippable b k,
    Derive a k,
    Derive b k,
    GaloisField k
  ) =>
  Game a k ->
  Game b k ->
  Game ('TProd a b) k
prod_game (Single iso) g2 = g2 +> iso'
  where
    iso' = prodI iso idI `seqI` prodLInputI
prod_game (Split iso g1a g1b) g2 =
  Split iso' (prod_game g1a g2) (prod_game g1b g2)
  where
    iso' = prodI iso idI `seqI` prodLSumI

basic_game2 :: (GaloisField k) => Game ('TProd 'TField 'TField) k
basic_game2 = prod_game field_game field_game

basic_test2 :: (GaloisField k) => Comp 'TField k
basic_test2 =
  do
    p <- decode basic_game2
    fst_pair p

t3 :: F_BN128
t3 = comp_interp basic_test2 [88, 23] Map.empty -- fst (23, 88) = 23

basic_game3 :: (GaloisField k) => Game ('TProd ('TProd 'TField 'TField) 'TField) k
basic_game3 =
  prod_game
    (prod_game field_game field_game)
    field_game

basic_test3 :: (GaloisField k) => Comp 'TField k
basic_test3 =
  do
    p <- decode basic_game3
    p2 <- fst_pair p
    snd_pair p2

t4 :: F_BN128
t4 = comp_interp basic_test3 [0, 1, 2] Map.empty

{---------------------------------------------------------
  Generic Games
 ---------------------------------------------------------}

class Gameable (a :: Ty) k where
  mkGame :: Game a k

instance Gameable 'TField p where
  mkGame = field_game

instance (GaloisField k) => Gameable 'TBool k where
  mkGame = bool_game

instance (GaloisField k) => Gameable 'TUnit k where
  mkGame = unit_game

instance
  ( Typeable a,
    Typeable b,
    Zippable a k,
    Zippable b k,
    Derive a k,
    Derive b k,
    Gameable a k,
    Gameable b k,
    GaloisField k
  ) =>
  Gameable ('TProd a b) k
  where
  mkGame = prod_game mkGame mkGame

instance
  ( Typeable a,
    Typeable b,
    Zippable a k,
    Zippable b k,
    Derive a k,
    Derive b k,
    Gameable a k,
    Gameable b k,
    GaloisField k
  ) =>
  Gameable ('TSum a b) k
  where
  mkGame = sum_game mkGame mkGame

gdecode :: (Gameable t k, GaloisField k) => Comp t k
gdecode = decode mkGame
