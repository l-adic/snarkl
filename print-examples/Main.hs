{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Foldable (traverse_)
import Snarkl.Field ()
import Snarkl.Toplevel (compileCompToTexp)
import Test.Snarkl.Unit.Programs
import Text.PrettyPrint.Leijen.Text

main :: IO ()
main = do
  traverse_
    printProg
    [ ("Program 1", prog1),
      ("Program 2", prog2 42),
      ("Program 3", prog3),
      ("Program 4", prog4),
      ("Program 5", prog5),
      ("Program 6", prog6),
      ("Program 7", prog7),
      ("Program 8", prog8),
      ("Program 11", prog11),
      ("Program 13", prog13),
      ("Program 14", prog14),
      ("Program 15", prog15),
      ("Program 26", prog26),
      ("Program 27", prog27),
      ("Program 28", prog28),
      ("Program 29", prog29),
      ("Program 30", prog30),
      ("Program 31", prog31),
      ("Program 34", prog34),
      ("Program 35", prog35),
      ("Program 36", prog36)
    ]
  traverse_
    printProg
    [ ("Bool Program 10", bool_prog10),
      ("Bool Program 12", bool_prog12),
      ("Bool Program 16", bool_prog16),
      ("Bool Program 17", bool_prog17),
      ("Bool Program 18", bool_prog18),
      ("Bool Program 19", bool_prog19),
      ("Bool Program 20", bool_prog20),
      ("Bool Program 21", bool_prog21),
      ("Bool Program 22", bool_prog22),
      ("Bool Program 23", bool_prog23),
      ("Bool Program 24", bool_prog24),
      ("Bool Program 25", bool_prog25),
      ("Bool Program 32", bool_prog32),
      ("Bool Program 33", bool_prog33)
    ]
  where
    printProg (name, prog) = do
      let texp = compileCompToTexp prog
      -- this is just a sanity check because of the new Eq instances
      if texp /= texp
        then putStrLn ("--| " <> name <> " (FAILED)")
        else do
          putStrLn (replicate 80 '-')
          putStrLn "\n"
          putStrLn ("--| " <> name)
          putStrLn "\n"
          print $ pretty texp
          putStrLn "\n"
