{-# LANGUAGE RebindableSyntax #-}

module Main where

import Snarkl.CLI (defaultMain)
import Snarkl.Field (F_BN128)
import Snarkl.Language.Prelude
import Snarkl.Toplevel (Comp, Ty (TField))
import qualified Test.Snarkl.Unit.Programs as Programs
import Prelude hiding (return, (+), (>>=))

main :: IO ()
main = defaultMain "prog" Programs.prog1
