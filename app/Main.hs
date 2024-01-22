{-# LANGUAGE RebindableSyntax #-}

module Main where

import Snarkl.CLI (defaultMain)
import Snarkl.Field (F_BN128)
import Snarkl.Language.Prelude
import Snarkl.Toplevel (Comp, Ty (TField))
import qualified Test.Snarkl.Unit.Programs as Programs
import Prelude hiding (return, (+), (>>=))

main :: IO ()
main = defaultMain "prog" (prog :: Comp 'TField F_BN128)

prog :: Comp 'TField F_BN128
prog = do
  v <- fresh_public_input
  _ <- assert (v `eq` fromField 42)
  return $ v + fromField 1
