{-# LANGUAGE TypeApplications #-}

-- Copy/pasted more or less from https://wiki.haskell.org/Sudoku#Simple_solver
module Main where

import Data.Array.IO (IOArray, getElems, newArray, readArray, writeArray)
import Data.Field.Galois (GaloisField)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.JSONLines (NoHeader (..), fromJSONLines, toJSONLines)
import Data.List (union, (\\))
import qualified Data.Map as Map
import Snarkl.CLI.Common (readFileLines, writeFileWithDir)
import Snarkl.Common (InputAssignment (..), Var (..), splitInputVars)
import Snarkl.Field (F_BN128)

main :: IO ()
main =
  let inputsFP = "./snarkl-output/sudoku-inputs.jsonl"
      assignmentsFP = "./snarkl-output/sudoku-assignments.jsonl"
   in defaultMain inputsFP assignmentsFP examplePuzzle

examplePuzzle :: [Int]
examplePuzzle =
  concat
    [ [0, 6, 0, 1, 0, 4, 0, 5, 0],
      [0, 0, 8, 3, 0, 5, 6, 0, 0],
      [2, 0, 0, 0, 0, 0, 0, 0, 1],
      [8, 0, 0, 4, 0, 7, 0, 0, 6],
      [0, 0, 6, 0, 0, 0, 3, 0, 0],
      [7, 0, 0, 9, 0, 1, 0, 0, 4],
      [5, 0, 0, 0, 0, 0, 0, 0, 2],
      [0, 0, 7, 2, 0, 6, 9, 0, 0],
      [0, 4, 0, 5, 0, 8, 0, 7, 0]
    ]

defaultMain ::
  FilePath ->
  FilePath ->
  [Int] ->
  IO ()
defaultMain inputsFP assignmentsFP publicInputs = do
  putStrLn $ "Reading inputs from " <> inputsFP
  eInputs <- fromJSONLines <$> readFileLines inputsFP
  let (_, privateInputs, [out]) = either error splitInputVars eInputs
  putStrLn "Solving puzzle..."
  m <- solvePuzzle publicInputs
  let assignments = mkAssignments @F_BN128 publicInputs privateInputs m out
  writeFileWithDir assignmentsFP (toJSONLines $ NoHeader assignments)
  putStrLn $ "Writing assignments to " <> assignmentsFP

mkAssignments ::
  (GaloisField k) =>
  [Int] ->
  Map.Map String Var ->
  Map.Map (Int, Int) Int ->
  Var ->
  [InputAssignment k]
mkAssignments publicInputs privateInputs m out =
  let mkVarName (i, j) = "x_" <> show (i, j)
      privateAssignments =
        [ PrivateInputAssignment name var value
          | (name, var) <- Map.toList privateInputs,
            (pos, value) <- Map.toList m,
            mkVarName pos == name
        ]
      publicAssignments = zipWith PublicInputAssignment (Var <$> [0 ..]) publicInputs
   in fmap fromIntegral <$> publicAssignments <> privateAssignments <> [OutputAssignment out 1]

mkPuzzleMap :: [Int] -> Map.Map (Int, Int) Int
mkPuzzleMap xs = Map.fromList $ zip [(x, y) | x <- [0 .. 8], y <- [0 .. 8]] xs

solvePuzzle :: [Int] -> IO (Map.Map (Int, Int) Int)
solvePuzzle inputs =
  mkPuzzleMap <$> do
    ref <- newIORef []
    a <- newArray (1, 81) 0
    readSudokuBoard a inputs
    solve ref a (1, 1)
    readIORef ref

type SudokuBoard = IOArray Int Int

readSudokuBoard :: SudokuBoard -> [Int] -> IO ()
readSudokuBoard a xs = sequence_ $ do
  (i, n) <- zip [1 .. 81] xs
  return $ writeArray a i n

--  (i, ys) <- zip [1 .. 9] xs
--  (j, n) <- zip [1 .. 9] ys
--  return $ writeBoard a (j, i) n

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
