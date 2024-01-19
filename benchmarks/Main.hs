{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Main where

import Criterion.Main
import Data.Field.Galois (GaloisField, Prime)
import Data.Typeable (Typeable)
import Harness
import Snarkl.Compile (SimplParam (..))
import qualified Snarkl.Example.Keccak as Keccak
import qualified Snarkl.Example.List as List
import qualified Snarkl.Example.Matrix as Matrix
import Snarkl.Field (F_BN128, P_BN128)
import Snarkl.Language.Prelude (Comp, fromField)

mk_bgroup :: (Typeable ty) => String -> Comp ty F_BN128 -> [Int] -> F_BN128 -> Benchmark
mk_bgroup nm mf inputs result =
  bgroup
    nm
    [ bench (nm ++ "-elaborate") $ nfIO $ test_texp mf,
      bench (nm ++ "-constraints") $ nfIO $ test_constraints mf,
      bench (nm ++ "-simplify") $ nfIO $ test_simplify mf,
      bench (nm ++ "-r1cs") $ nfIO $ test_r1csgen Simplify mf,
      bench (nm ++ "-witgen") $ nfIO $ test_witgen Simplify mf inputs,
      bench (nm ++ "-keygen") $ nfIO $ test_keygen Simplify mf inputs,
      bench (nm ++ "-verif") $ nfIO $ test_crypto Simplify mf inputs,
      bench (nm ++ "-full") $ nfIO $ test_numconstrs Simplify mf inputs result
    ]

the_benchmarks =
  [ mk_bgroup "keccak800" (Keccak.keccak1 22) Keccak.input_vals 1,
    mk_bgroup "map-list" List.test_listN (90 : take 100 [0 ..]) 90,
    mk_bgroup "fixed-matrix600" (Matrix.test1 600) [0 .. 599] 754740000,
    mk_bgroup
      "input-matrix70"
      (Matrix.test2 70)
      (Matrix.t2_m0 4900 ++ Matrix.t2_m1 4900)
      (2048215153250 :: F_BN128)
  ]

run_benchmarks =
  defaultMain the_benchmarks

main = run_benchmarks
