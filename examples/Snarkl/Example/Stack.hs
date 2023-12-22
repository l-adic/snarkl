{-# LANGUAGE RebindableSyntax #-}

module Snarkl.Example.Stack where

import Data.Field.Galois (Prime)
import Data.Typeable
import GHC.TypeLits (KnownNat)
import Snarkl.Compile
import Snarkl.Example.List
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

type TStack a = TList a

type Stack a p = TExp (TStack a) (Prime p)

empty_stack :: (Typeable a, KnownNat p) => Comp (TStack a) p
empty_stack = nil

push_stack :: (Typeable a, KnownNat p) => TExp a (Prime p) -> Stack a p -> Comp (TStack a) p
push_stack p q = cons p q

pop_stack :: (Derive a p, Zippable a p, Typeable a, KnownNat p) => Stack a p -> Comp (TStack a) p
pop_stack f = tail_list f

top_stack :: (Derive a p, Zippable a p, Typeable a, KnownNat p) => TExp a (Prime p) -> Stack a p -> Comp a p
top_stack def e = head_list def e

is_empty_stack :: (Typeable a, KnownNat p) => Stack a p -> Comp 'TBool p
is_empty_stack s =
  case_list s (return true) (\_ _ -> return false)

---Test Examples---

stack1 :: (KnownNat p) => Comp (TStack 'TField) p
stack1 =
  do
    tl <- empty_stack
    tl' <- push_stack (fromPrimeField 15) tl
    push_stack (fromPrimeField 99) tl'

stack2 :: (KnownNat p) => Comp (TStack 'TField) p
stack2 =
  do
    tl <- empty_stack
    tl' <- push_stack (fromPrimeField 1) tl
    tl'' <- push_stack (fromPrimeField 12) tl'
    push_stack (fromPrimeField 89) tl''

-- top_stack on empty stack
test_top1 :: (KnownNat p) => Comp 'TField p
test_top1 =
  do
    s1 <- stack1
    s2 <- pop_stack s1
    s3 <- pop_stack s2
    top_stack (fromPrimeField 1) s3

-- top_stack on non-empty stack
test_top2 :: (KnownNat p) => Comp 'TField p
test_top2 =
  do
    s1 <- stack1
    top_stack (fromPrimeField 1) s1

-- is_empty_stack on an empty stack
test_empty_stack1 :: (KnownNat p) => Comp 'TBool p
test_empty_stack1 =
  do
    s1 <- stack1
    s2 <- pop_stack s1
    s3 <- pop_stack s2
    is_empty_stack s3

-- is_empty_stack on non-empty stack
test_empty_stack2 :: (KnownNat p) => Comp 'TBool p
test_empty_stack2 =
  do
    s1 <- stack1
    is_empty_stack s1
