{-# LANGUAGE RebindableSyntax #-}

module Snarkl.Example.List where

import Data.Field.Galois (Prime)
import Data.Typeable
import GHC.TypeLits (KnownNat)
import Snarkl.Language.Syntax
import Snarkl.Language.SyntaxMonad
import Snarkl.Language.TExpr
import Snarkl.Toplevel
import Prelude hiding
  ( negate,
    return,
    (&&),
    (*),
    (+),
    (-),
    (/),
    (>>),
    (>>=),
  )

type TF a = 'TFSum ('TFConst 'TUnit) ('TFProd ('TFConst a) 'TFId)

type TList a = 'TMu (TF a)

type List a p = TExp (TList a) (Prime p)

nil :: (Typeable a, KnownNat p) => Comp (TList a) p
nil = do
  t <- inl unit
  roll t

cons :: (Typeable a, KnownNat p) => TExp a (Prime p) -> List a p -> Comp (TList a) p
cons f t =
  do
    p <- pair f t
    t' <- inr p
    roll t'

case_list ::
  ( Typeable a,
    Typeable ty,
    Zippable ty p,
    KnownNat p
  ) =>
  List a p ->
  Comp ty p ->
  (TExp a (Prime p) -> List a p -> Comp ty p) ->
  Comp ty p
case_list t f_nil f_cons =
  do
    t' <- unroll t
    case_sum (\_ -> f_nil) go t'
  where
    go p =
      do
        e1 <- fst_pair p
        e2 <- snd_pair p
        f_cons e1 e2

head_list ::
  ( Typeable a,
    Zippable a p,
    Derive a p,
    KnownNat p
  ) =>
  TExp a (Prime p) ->
  List a p ->
  Comp a p
head_list def l =
  case_list
    l
    (return def)
    (\hd _ -> return hd)

tail_list ::
  ( Typeable a,
    Zippable a p,
    Derive a p,
    KnownNat p
  ) =>
  List a p ->
  Comp (TList a) p
tail_list l =
  case_list
    l
    nil
    (\_ tl -> return tl)

{- rev [] = []
   rev (hd : tl) = rev tl ++ [hd]
 -}

app_list ::
  ( Typeable a,
    Zippable a p,
    Derive a p,
    KnownNat p
  ) =>
  List a p ->
  List a p ->
  Comp (TList a) p
app_list l1 l2 = fix go l1
  where
    go self l0 =
      case_list
        l0
        (return l2)
        ( \a l0' -> do
            l0'' <- self l0'
            cons a l0''
        )

rev_list ::
  ( Typeable a,
    Zippable a p,
    Derive a p,
    KnownNat p
  ) =>
  List a p ->
  Comp (TList a) p
rev_list l = fix go l
  where
    go self l0 =
      case_list
        l0
        nil
        ( \a l0' -> do
            l0'' <- self l0'
            a_tl <- nil
            a_l <- cons a a_tl
            app_list l0'' a_l
        )

map_list ::
  ( Typeable a,
    Zippable a p,
    Derive a p,
    Typeable b,
    Zippable b p,
    Derive b p,
    KnownNat p
  ) =>
  (TExp a (Prime p) -> Comp b p) ->
  List a p ->
  Comp (TList b) p
map_list f l =
  fix go l
  where
    go self l0 =
      case_list
        l0
        nil
        ( \hd tl ->
            do
              hd' <- f hd
              tl' <- self tl
              cons hd' tl'
        )

last_list ::
  (Typeable a, Zippable a p, Derive a p, KnownNat p) =>
  TExp a (Prime p) ->
  List a p ->
  Comp a p
last_list def l =
  fix go l
  where
    go self l0 =
      case_list
        l0
        (return def)
        ( \hd tl ->
            case_list
              tl
              (return hd)
              (\_ _ -> self tl)
        )

{------------------------------------------------
 A couple (very simple) test cases
 ------------------------------------------------}

list1 :: (KnownNat p) => Comp (TList 'TField) p
list1 =
  do
    tl <- nil
    tl' <- cons (exp_of_int 23) tl
    cons (exp_of_int 33) tl'

inc_elem e = return $ exp_of_int 1 + e

list2 :: (KnownNat p) => Comp (TList 'TField) p
list2 =
  do
    l <- list1
    map_list inc_elem l

list_comp3 :: (KnownNat p) => Comp 'TField p
list_comp3 =
  do
    b <- fresh_input
    l <- nil
    l' <- cons (fromPrimeField 23) l
    l'' <- cons (fromPrimeField 33) l'
    l2 <- if return b then return l'' else return l
    l3 <- map_list inc_elem l2
    l4 <- tail_list l3
    head_list (fromPrimeField 0) l4

list_comp4 :: (KnownNat p) => Comp 'TField p
list_comp4 =
  do
    l <- list2
    last_list (fromPrimeField 0) l

listN :: (Typeable a, Zippable a p, Derive a p, KnownNat p) => TExp 'TField (Prime p) -> Comp (TList a) p
listN n = fixN 100 go n
  where
    go self n0 = do
      x <- fresh_input
      tl <- self (n0 - fromPrimeField 1)
      if return (eq n0 (fromPrimeField 0)) then nil else cons x tl

test_listN :: (KnownNat p) => Comp 'TField p
test_listN = do
  n <- fresh_input
  l1 <- listN n
  l2 <- map_list inc_elem l1
  last_list (fromPrimeField 99) l2
