{-# LANGUAGE RebindableSyntax #-}

module Snarkl.Example.Stack where

import Data.Field.Galois (GaloisField, Prime)
import Data.Typeable
import GHC.TypeLits (KnownNat)
import Snarkl.Compile
import Snarkl.Example.List
import Snarkl.Language.Prelude
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

type Stack a k = TExp (TStack a) k

empty_stack :: (Typeable a, GaloisField k) => Comp (TStack a) k
empty_stack = nil

push_stack :: (Typeable a, GaloisField k) => TExp a k -> Stack a k -> Comp (TStack a) k
push_stack p q = cons p q

pop_stack :: (Derive a k, Zippable a k, Typeable a, GaloisField k) => Stack a k -> Comp (TStack a) k
pop_stack f = tail_list f

top_stack :: (Derive a k, Zippable a k, Typeable a, GaloisField k) => TExp a k -> Stack a k -> Comp a k
top_stack def e = head_list def e

is_empty_stack :: (Typeable a, GaloisField k) => Stack a k -> Comp 'TBool k
is_empty_stack s =
  case_list s (return true) (\_ _ -> return false)

---Test Examples---

stack1 :: (GaloisField k) => Comp (TStack 'TField) k
stack1 =
  do
    tl <- empty_stack
    tl' <- push_stack (fromField 15) tl
    push_stack (fromField 99) tl'

stack2 :: (GaloisField k) => Comp (TStack 'TField) k
stack2 =
  do
    tl <- empty_stack
    tl' <- push_stack (fromField 1) tl
    tl'' <- push_stack (fromField 12) tl'
    push_stack (fromField 89) tl''

-- top_stack on empty stack
test_top1 :: (GaloisField k) => Comp 'TField k
test_top1 =
  do
    s1 <- stack1
    s2 <- pop_stack s1
    s3 <- pop_stack s2
    top_stack (fromField 1) s3

-- top_stack on non-empty stack
test_top2 :: (GaloisField k) => Comp 'TField k
test_top2 =
  do
    s1 <- stack1
    top_stack (fromField 1) s1

-- is_empty_stack on an empty stack
test_empty_stack1 :: (GaloisField k) => Comp 'TBool k
test_empty_stack1 =
  do
    s1 <- stack1
    s2 <- pop_stack s1
    s3 <- pop_stack s2
    is_empty_stack s3

-- is_empty_stack on non-empty stack
test_empty_stack2 :: (GaloisField k) => Comp 'TBool k
test_empty_stack2 =
  do
    s1 <- stack1
    is_empty_stack s1
