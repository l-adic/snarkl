{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snarkl.Example.Games where

import Data.Field.Galois (Prime)
import Data.Typeable
import GHC.TypeLits (KnownNat, Nat)
import Snarkl.Errors
import Snarkl.Field (F_BN128)
import Snarkl.Language.Syntax
import Snarkl.Language.SyntaxMonad
import Snarkl.Language.TExpr
import Snarkl.Toplevel
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

data ISO (t :: Ty) (s :: Ty) p = Iso
  { to :: TExp t (Prime p) -> Comp s p,
    from :: TExp s (Prime p) -> Comp t p
  }

data Game :: Ty -> Nat -> * where
  Single ::
    forall (s :: Ty) (t :: Ty) p.
    ( Typeable s,
      Typeable t
    ) =>
    ISO t s p ->
    Game t p
  Split ::
    forall (t1 :: Ty) (t2 :: Ty) (t :: Ty) p.
    ( Typeable t1,
      Typeable t2,
      Typeable t,
      Zippable t1 p,
      Zippable t2 p,
      Zippable t p,
      Derive t1 p,
      Derive t2 p
    ) =>
    ISO t ('TSum t1 t2) p ->
    Game t1 p ->
    Game t2 p ->
    Game t p

decode :: (KnownNat p) => Game t p -> Comp t p
decode (Single (Iso _ bld)) =
  do
    x <- fresh_input
    bld x
decode (Split (Iso _ bld) g1 g2) =
  do
    x <- fresh_input
    e1 <- decode g1
    e2 <- decode g2
    s1 <- inl e1
    s2 <- inr e2
    v1 <- bld s1
    v2 <- bld s2
    if return x then return v2 else return v1

field_game :: Game 'TField p
field_game = Single (Iso return return)

bool_game :: (KnownNat p) => Game 'TBool p
bool_game =
  Single
    ( Iso
        (\be -> if return be then return (fromPrimeField 1) else return (fromPrimeField 0))
        (\te -> if return (zeq te) then return false else return true)
    )

unit_game :: (KnownNat p) => Game 'TUnit p
unit_game = Single (Iso (\_ -> return (fromPrimeField 1)) (\(_ :: TExp 'TField (Prime p)) -> return unit))

fail_game :: (Typeable ty) => Game ty p
fail_game =
  Single
    ( Iso
        (\_ -> failWith $ ErrMsg "fail-games can't encode")
        ( \(_ :: TExp 'TField (Prime p)) ->
            failWith $ ErrMsg "fail-games can't decode"
        )
    )

sum_game ::
  ( Typeable t1,
    Typeable t2,
    Zippable t1 p,
    Zippable t2 p,
    Derive t1 p,
    Derive t2 p,
    KnownNat p
  ) =>
  Game t1 p ->
  Game t2 p ->
  Game ('TSum t1 t2) p
sum_game g1 g2 =
  Split (Iso return return) g1 g2

basic_game :: (KnownNat p) => Game ('TSum 'TField 'TField) p
basic_game = sum_game field_game field_game

basic_test :: (KnownNat p) => Comp 'TField p
basic_test =
  do
    s <- decode basic_game
    case_sum return return s

t1 :: F_BN128
t1 = comp_interp basic_test [0, 23, 88] -- 23

t2 :: F_BN128
t2 = comp_interp basic_test [1, 23, 88] -- 88

(+>) ::
  ( Typeable t,
    Typeable s,
    Zippable t p,
    Zippable s p
  ) =>
  Game t p ->
  ISO s t p ->
  Game s p
(Single j) +> i = Single (i `seqI` j)
(Split j g1 g2) +> i = Split (i `seqI` j) g1 g2

idI :: ISO a a p
idI = Iso return return

prodI ::
  ( Typeable a,
    Typeable b,
    Typeable c,
    Typeable d,
    KnownNat p
  ) =>
  ISO a b p ->
  ISO c d p ->
  ISO ('TProd a c) ('TProd b d) p
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
    KnownNat p
  ) =>
  ISO ('TProd a b) b p
prodLInputI =
  Iso
    snd_pair
    ( \b -> do
        a <- fresh_input
        pair a b
    )

prodLSumI ::
  ( Typeable a,
    Typeable b,
    Typeable c,
    Zippable a p,
    Zippable b p,
    Zippable c p,
    Derive a p,
    Derive b p,
    Derive c p,
    KnownNat p
  ) =>
  ISO ('TProd ('TSum b c) a) ('TSum ('TProd b a) ('TProd c a)) p
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
    Zippable a p,
    Zippable b p,
    Derive a p,
    Derive b p,
    KnownNat p
  ) =>
  Game a p ->
  Game b p ->
  Game ('TProd a b) p
prod_game (Single iso) g2 = g2 +> iso'
  where
    iso' = prodI iso idI `seqI` prodLInputI
prod_game (Split iso g1a g1b) g2 =
  Split iso' (prod_game g1a g2) (prod_game g1b g2)
  where
    iso' = prodI iso idI `seqI` prodLSumI

basic_game2 :: (KnownNat p) => Game ('TProd 'TField 'TField) p
basic_game2 = prod_game field_game field_game

basic_test2 :: (KnownNat p) => Comp 'TField p
basic_test2 =
  do
    p <- decode basic_game2
    fst_pair p

t3 :: F_BN128
t3 = comp_interp basic_test2 [88, 23] -- fst (23, 88) = 23

basic_game3 :: (KnownNat p) => Game ('TProd ('TProd 'TField 'TField) 'TField) p
basic_game3 =
  prod_game
    (prod_game field_game field_game)
    field_game

basic_test3 :: (KnownNat p) => Comp 'TField p
basic_test3 =
  do
    p <- decode basic_game3
    p2 <- fst_pair p
    snd_pair p2

t4 :: F_BN128
t4 = comp_interp basic_test3 [0, 1, 2]

{---------------------------------------------------------
  Generic Games
 ---------------------------------------------------------}

class Gameable (a :: Ty) (p :: Nat) where
  mkGame :: Game a p

instance Gameable 'TField p where
  mkGame = field_game

instance (KnownNat p) => Gameable 'TBool p where
  mkGame = bool_game

instance (KnownNat p) => Gameable 'TUnit p where
  mkGame = unit_game

instance
  ( Typeable a,
    Typeable b,
    Zippable a p,
    Zippable b p,
    Derive a p,
    Derive b p,
    Gameable a p,
    Gameable b p,
    KnownNat p
  ) =>
  Gameable ('TProd a b) p
  where
  mkGame = prod_game mkGame mkGame

instance
  ( Typeable a,
    Typeable b,
    Zippable a p,
    Zippable b p,
    Derive a p,
    Derive b p,
    Gameable a p,
    Gameable b p,
    KnownNat p
  ) =>
  Gameable ('TSum a b) p
  where
  mkGame = sum_game mkGame mkGame

gdecode :: (Gameable t p, KnownNat p) => Comp t p
gdecode = decode mkGame
