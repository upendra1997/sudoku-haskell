module Lib
  ( Cell,
    Row,
    Sudoku,
    sudokuSize,
    getSolutions,
    showSudoku,
  )
where

import           Data.List (nub, transpose)

type Cell = Int

type SudokuSize = Int

sudokuSize :: SudokuSize
sudokuSize = 9

blockSize :: Int
blockSize = 3

showRow :: Show a => [a] -> String
showRow row = unwords $ show <$> row

showSudoku :: Show a => [[a]] -> String
showSudoku sudoku = unlines $ showRow <$> sudoku

type Row = [Int]

type X = Int

type Y = Int

type Coord = (X, Y)

type Sudoku = [Row]

possibilities :: Cell -> [Cell]
possibilities 0 = [1 .. 9]
possibilities n = [n]

crossProduct :: [[a]] -> [[a]]
crossProduct []             = []
crossProduct [a]            = [[x] | x <- a]
crossProduct (array : rest) = (:) <$> array <*> crossProduct rest

rows :: [[a]] -> [[[a]]]
rows = fmap (replicate sudokuSize)

columns :: [[a]] -> [[[a]]]
columns = transpose . rows . transpose

coordinates :: [[Coord]]
coordinates = [[(r, c) | c <- [0 .. sudokuSize - 1]] | r <- [0 .. sudokuSize - 1]]

blockCoordinates :: [[(Coord, Coord)]]
blockCoordinates = (fmap . fmap) (\(x, y) -> (start x y, end x y)) coordinates
  where
    start x' y' = (3 * (x' `div` blockSize), 3 * (y' `div` blockSize))
    end x' y' = (\(x'', y'') -> (x'' + blockSize, y'' + blockSize)) $ start x' y'

slice :: Int -> Int -> [a] -> [a]
slice start end = drop start . take end

slice2D :: [[a]] -> Int -> Int -> Int -> Int -> [[a]]
slice2D sudoku startRow endRow startCol endCol = slice startRow endRow $ slice startCol endCol <$> sudoku

blocks :: Sudoku -> [[[[Int]]]]
blocks sudoku = (fmap . fmap) block blockCoordinates
  where
    getSlice = slice2D sudoku
    block ((startRow, startCol), (endRow, endCol)) = getSlice startRow endRow startCol endCol

valid :: Sudoku -> Bool
valid sudoku = (all . all) (== 9) $ (fmap . fmap) (length . nub) all'
  where
    add' = (zipWith . zipWith) (++)
    all' = add' blocks' $ add' rows' cols'
    rows' = rows sudoku
    cols' = columns sudoku
    blocks' = fmap concat <$> blocks sudoku

getSolutions :: Sudoku -> [Sudoku]
getSolutions x = filter valid allSudokus
  where
    allSudokus = crossProduct (crossProduct . fmap possibilities <$> x)

-- sudoku = [[9, 2, 6, 3, 4, 0, 7, 0, 1],
--           [0, 5, 0, 0, 2, 6, 4, 0, 9],
--           [0, 7, 0, 8, 0, 1, 0, 0, 0],
--           [0, 0, 0, 9, 0, 0, 2, 0, 7],
--           [3, 4, 2, 0, 0, 0, 0, 0, 5],
--           [1, 0, 0, 0, 0, 0, 8, 0, 0],
--           [6, 8, 5, 4, 0, 0, 0, 1, 2],
--           [0, 0, 4, 0, 0, 2, 9, 0, 0],
--           [0, 1, 0, 5, 3, 8, 0, 7, 0]]
