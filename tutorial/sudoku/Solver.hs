module Solver (solvePuzzle) where

import Data.Array.IO
import Data.IORef
import Data.List

solvePuzzle :: [[Int]] -> IO ([Int])
solvePuzzle inputs = do
  ref <- newIORef []
  a <- newArray (1, 81) 0
  readSudokuBoard a inputs
  solve ref a (1, 1)
  readIORef ref

type SudokuBoard = IOArray Int Int

readSudokuBoard :: SudokuBoard -> [[Int]] -> IO ()
readSudokuBoard a xs = sequence_ $ do
  (i, ys) <- zip [1 .. 9] xs
  (j, n) <- zip [1 .. 9] ys
  return $ writeBoard a (j, i) n

-- the meat of the program.  Checks the current square.
-- If 0, then get the list of nums and try to "solve' "
-- Otherwise, go to the next square.
solve :: IORef [Int] -> SudokuBoard -> (Int, Int) -> IO ()
solve ref a (10, y) = solve ref a (1, y + 1)
solve ref a (_, 10) = writeIORef ref =<< getElems a
solve ref a (x, y) = do
  v <- readBoard a (x, y)
  case v of
    0 -> availableNums a (x, y) >>= solve' (x, y)
    _ -> solve ref a (x + 1, y)
  where
    -- solve' handles the backtacking
    solve' (_x, _y) [] = return ()
    solve' (_x, _y) (v : vs) = do
      writeBoard a (_x, _y) v -- put a guess onto the board
      solve ref a (_x + 1, _y)
      writeBoard a (_x, _y) 0 -- remove the guess from the board
      solve' (_x, _y) vs -- recurse over the remainder of the list

-- get the "taken" numbers from a row, col or box.
getRowNums :: SudokuBoard -> Int -> IO [Int]
getRowNums a y = sequence [readBoard a (x', y) | x' <- [1 .. 9]]

getColNums :: SudokuBoard -> Int -> IO [Int]
getColNums a x = sequence [readBoard a (x, y') | y' <- [1 .. 9]]

getBoxNums :: SudokuBoard -> (Int, Int) -> IO [Int]
getBoxNums a (x, y) = sequence [readBoard a (x' + u, y' + v) | u <- [0 .. 2], v <- [0 .. 2]]
  where
    x' = (3 * ((x - 1) `quot` 3)) + 1
    y' = (3 * ((y - 1) `quot` 3)) + 1

-- return the numbers that are available for a particular square
availableNums :: SudokuBoard -> (Int, Int) -> IO [Int]
availableNums a (x, y) = do
  r <- getRowNums a y
  c <- getColNums a x
  b <- getBoxNums a (x, y)
  return $ [0 .. 9] \\ (r `union` c `union` b)

-- aliases of read and write array that flatten the index
readBoard :: SudokuBoard -> (Int, Int) -> IO Int
readBoard a (x, y) = readArray a (x + 9 * (y - 1))

writeBoard :: SudokuBoard -> (Int, Int) -> Int -> IO ()
writeBoard a (x, y) e = writeArray a (x + 9 * (y - 1)) e