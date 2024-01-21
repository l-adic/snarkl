```haskell

{-# LANGUAGE TypeApplications #-}

import Sudoku (validatePuzzle)
import Snarkl.Field (F_BN128)
import Snarkl.CLI (defaultMain)

main :: IO ()
main = defaultMain "sudoku" (validatePuzzle @F_BN128 [(i,i) | i <- [0..9]])

```