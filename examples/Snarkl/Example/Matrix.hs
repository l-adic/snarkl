{-# LANGUAGE RebindableSyntax #-}

module Snarkl.Example.Matrix where

import Data.Field.Galois (GaloisField, Prime)
import GHC.TypeLits (KnownNat)
import Snarkl.Language.Prelude
import Snarkl.Toplevel (comp_interp)
import Prelude hiding
  ( fromRational,
    negate,
    not,
    return,
    (&&),
    (*),
    (+),
    (-),
    (/),
    (>>),
    (>>=),
  )
import qualified Prelude as P

type Matrix = TExp ('TArr ('TArr 'TField))

new_matrix n m = arr2 n m

new_rowvec n = arr n

new_colvec n = arr n

input_matrix n m = input_arr2 n m

input_rowvec n = input_arr n

input_colvec n = input_arr n

type FixedMatrix k = Int -> Int -> k

-- v0 + v1 + .. + v(n-1)
sum_vec n v = do
  iterM
    (dec n)
    ( \i acc -> do
        a <- get (v, i)
        return $ a + acc
    )
    (fromField 0)

sum_mat n m mat = do
  iterM
    (dec n)
    ( \i acc -> do
        a <-
          iterM
            (dec m)
            ( \j acc' -> do
                mat_elem <- get2 (mat, i, j)
                return $ mat_elem + acc'
            )
            (fromField 0)
        return $ a + acc
    )
    (fromField 0)

input_matrix_mult n m p = do
  a <- input_matrix n m
  b <- input_matrix m p
  c <- new_matrix n p

  forall
    [0 .. dec n]
    ( \i -> do
        forall
          [0 .. dec p]
          ( \j -> do
              res <-
                iterM
                  (dec m)
                  ( \k acc -> do
                      aElem <- get2 (a, i, k)
                      bElem <- get2 (b, k, j)
                      return $ (bElem * aElem) + acc
                  )
                  (fromField 0)
              set2 (c, i, j) res
          )
    )
  sum_mat n n c

-- Pinocchio's "Fixed Matrix" microbenchmark [p9]
matrix_colvec_mult fm n = do
  v <- input_colvec n
  v' <- new_colvec n

  -- multiply
  forall
    [0 .. dec n]
    ( \i -> do
        res <-
          iterM
            (dec n)
            ( \j acc -> do
                a <- get (v, j)
                return $ fm i j * a + acc
            )
            (fromField 0)
        set (v', i) res
    )

  -- return an output that's dependent on the entire vector v'
  sum_vec n v'

{------------------------------------------------
Test cases
------------------------------------------------}

test1 :: (GaloisField k) => Int -> Comp 'TField k
test1 n = matrix_colvec_mult (\_ _ -> fromField 7) n

interp1 n = comp_interp (test1 n) (map fromIntegral [0 .. dec n])

t2_m0 n = map fromIntegral [0 .. dec n]

t2_m1 n = reverse (t2_m0 n)

test2 :: (GaloisField k) => Int -> Comp 'TField k
test2 n = input_matrix_mult n n n

interp2 n =
  comp_interp
    (test2 n)
    (t2_m0 (n P.* n) ++ t2_m1 (n P.* n))
