module Main where

import Data.Foldable (traverse_)
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Snarkl.Toplevel (texp_of_comp)
import UnitTests

main :: IO ()
main = do
  let printProg prog = do
        let doc = pretty $ texp_of_comp prog
            layout = layoutPretty defaultLayoutOptions doc
        putStrLn $ renderString layout
        putStrLn "\n"
   in traverse_
        printProg
        [ prog1,
          prog2 42,
          prog3,
          prog4,
          prog5,
          prog6,
          prog7,
          prog8
        ]
