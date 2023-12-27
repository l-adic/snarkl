module Main where

import Snarkl.Compile (SimplParam (NoSimplify))
import Snarkl.Field
import Snarkl.Toplevel (executeAndWriteArtifacts, r1cs_of_comp, serializeR1CSAsJson)
import qualified Test.Snarkl.Unit.Programs as Programs

{-

prog2 n =
  do
    e <- fresh_input
    let f i = exp_of_int i + e
    return $ bigsum n f

prog2 10 [1]
  == bigsum 10 (\n -> exp_of_int n + 1)
  == iter 10 (\n e -> (exp_of_int n + 1) + e) 0
  == (\e -> exp_of_int 10 + 1) $ iter 9 (\n e -> (exp_of_int n + 1) + e) 0
  == (\e -> exp_of_int 10 + 1) $ (\e -> exp_of_int 9 + 1) $ iter 8 (\n e -> (exp_of_int n + 1) + e) 0
  ...
  == (\e -> (exp_of_int 10 + 1) + e) $ (\e -> (exp_of_int 9 + 1) + e) $ ... $ (\e -> (exp_of_int 2 + 1) + e) $ (\e -> (exp_of_int 1 + 1) + e) $ (\n e -> (exp_of_int n + 1) + e) 0 0
  == (\e -> (exp_of_int 10 + 1) + e) $ (\e -> (exp_of_int 9 + 1) + e) $ ... $ (\e -> (exp_of_int 2 + 1) + e) $ (\e -> (exp_of_int 1 + 1) + e) 1
  == (\e -> (exp_of_int 10 + 1) + e) $ (\e -> (exp_of_int 9 + 1) + e) $ ... $ (\e -> (exp_of_int 2 + 1) + e) $ exp_of_int 1 + 1) + 1
  == (\e -> (exp_of_int 10 + 1) + e) $ (\e -> (exp_of_int 9 + 1) + e) $ ... $ (exp_of_int 2 + 1) + ((exp_of_int 1 + 1) + (exp_of_int 0 + 1))
  == sum_0^10 (\n -> exp_of_int n + 1)
  == sum_0^10 (\n -> exp_of_int n) + sum_0^10 (\n -> 1)
  == sum_0^10 (\n -> exp_of_int n) + 11
  == ((10 * 10 + 1) / 2) + 11
  == 66

-}

main :: IO ()
main = do
  executeAndWriteArtifacts "prog2" NoSimplify (Programs.prog2 10) [1 :: F_BN128]