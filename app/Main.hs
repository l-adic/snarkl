module Main where

import Snarkl.CLI (defaultMain)
import Snarkl.Field (F_BN128)
import Snarkl.Toplevel (Comp, Ty (TField))
import qualified Test.Snarkl.Unit.Programs as Programs

main :: IO ()
main = defaultMain "prog" (Programs.prog2 10 :: Comp 'TField F_BN128)
