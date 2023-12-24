module Main where

import Snarkl.Compile (SimplParam (NoSimplify))
import Snarkl.Toplevel (r1cs_of_comp, serializeR1CSAsJson)
import qualified Test.Snarkl.Unit.Programs as Programs

main :: IO ()
main = do
  let prog = Programs.prog1
  let r1cs = r1cs_of_comp NoSimplify prog
  serializeR1CSAsJson "r1cs.json" r1cs