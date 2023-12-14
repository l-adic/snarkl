module Main where

import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Snarkl.Toplevel (texp_of_comp)
import UnitTests (prog1, prog2, prog3)

main :: IO ()
main =
  let doc = pretty $ texp_of_comp prog1
      layout = layoutPretty defaultLayoutOptions doc
      prettyString = renderString layout
   in putStrLn prettyString
