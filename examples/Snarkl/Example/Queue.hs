{-# LANGUAGE RebindableSyntax #-}

module Snarkl.Example.Queue where

import Data.Field.Galois (GaloisField, Prime)
import Data.Typeable
import GHC.TypeLits (KnownNat)
import Snarkl.Example.List
import Snarkl.Example.Stack
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

type TQueue a = 'TProd (TStack a) (TStack a)

type Queue a k = TExp (TQueue a) k

empty_queue :: (Typeable a, GaloisField k) => Comp (TQueue a) k
empty_queue = do
  l <- empty_stack
  r <- empty_stack
  pair l r

enqueue ::
  (Zippable a k, Derive a k, Typeable a, GaloisField k) =>
  TExp a k ->
  Queue a k ->
  Comp (TQueue a) k
enqueue v q = do
  l <- fst_pair q
  r <- snd_pair q
  l' <- push_stack v l
  pair l' r

dequeue ::
  (Zippable a k, Derive a k, Typeable a, GaloisField k) =>
  Queue a k ->
  TExp a k ->
  Comp ('TProd a (TQueue a)) k
dequeue q def = do
  l <- fst_pair q
  r <- snd_pair q
  l_empty <- is_empty_stack l
  r_empty <- is_empty_stack r
  if return r_empty
    then
      if return l_empty
        then do
          pair def q
        else do
          l' <- nil
          pre_r <- rev_list l
          h <- top_stack def pre_r
          r' <- pop_stack pre_r
          q' <- pair l' r'
          pair h q'
    else do
      h <- top_stack def r
      r' <- pop_stack r
      p <- pair l r'
      pair h p

dequeue_rec ::
  (Zippable a k, Derive a k, Typeable a, GaloisField k) =>
  Queue a k ->
  TExp a k ->
  Comp ('TProd a (TQueue a)) k
dequeue_rec q def = fix go q
  where
    go self q0 = do
      l <- fst_pair q0
      r <- snd_pair q0
      l_empty <- is_empty_stack l
      r_empty <- is_empty_stack r
      if return r_empty
        then
          if return l_empty
            then do
              pair def q0
            else do
              l' <- nil
              r' <- rev_list l
              p' <- pair l' r'
              self p'
        else do
          h <- top_stack def r
          r' <- pop_stack r
          p <- pair l r'
          pair h p

is_empty q = do
  l <- fst_pair q
  r <- snd_pair q
  case_list
    l
    ( case_list
        r
        (return true)
        (\_ _ -> return false)
    )
    (\_ _ -> return false)

last_queue ::
  (Zippable a k, Derive a k, Typeable a, GaloisField k) =>
  Queue a k ->
  TExp a k ->
  Comp a k
last_queue q def = fixN 100 go q
  where
    go self p = do
      p_pair <- dequeue p def
      p_queue <- snd_pair p_pair
      p_top <- fst_pair p_pair
      b <- is_empty p_queue
      if return b
        then return p_top
        else self p_queue

map_queue f q = do
  lq <- fst_pair q
  rq <- snd_pair q
  lq' <- map_list f lq
  rq' <- map_list f rq
  pair lq' rq'

-----------------------------------------
-- Simple Examples------------------------
-----------------------------------------

-- queue with {nonempty stack, nonempty stack}
queue1 :: (GaloisField k) => Comp (TQueue 'TField) k
queue1 =
  do
    s1 <- stack1
    s2 <- stack2
    pair s1 s2

-- queue with {nonempty stack, empty stack}
queue2 :: (GaloisField k) => Comp (TQueue 'TField) k
queue2 =
  do
    s1 <- stack1
    s2 <- pop_stack s1
    s3 <- pop_stack s2
    s4 <- stack2
    pair s4 s3

queue_comp1 :: (GaloisField k) => Comp 'TField k
queue_comp1 =
  do
    q1 <- queue1
    q2 <- enqueue (fromField 1) q1
    q3 <- enqueue (fromField 3 + (fromField 4 / fromField 10)) q2
    sx <- fst_pair q3
    top_stack (fromField 0) sx

-- dequeue where input is queue with {nonempty, nonempty}
queue_comp2 :: (GaloisField k) => Comp 'TField k
queue_comp2 =
  do
    q1 <- queue1
    sx <- dequeue q1 (fromField 0)
    fst_pair sx

-- dequeue where input is queue with {nonempty, empty}
queue_comp3 :: (GaloisField k) => Comp 'TField k
queue_comp3 =
  do
    q1 <- queue2
    sx <- dequeue q1 (fromField 0)
    fst_pair sx

queueN :: (Typeable a, Zippable a k, Derive a k, GaloisField k) => TExp 'TField k -> Comp (TQueue a) k
queueN n = fixN 100 go n
  where
    go self n0 = do
      x <- fresh_public_input
      tl <- self (n0 - fromField 1)
      if return (eq n0 (fromField 0))
        then empty_queue
        else enqueue x tl

test_queueN :: (GaloisField k) => Comp 'TField k
test_queueN = do
  n <- fresh_public_input
  q1 <- queueN n
  q2 <- map_queue inc_elem q1
  last_queue q2 (fromField 105)
