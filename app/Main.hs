module Main (main) where

import Data.Char (digitToInt)
import Control.Monad (replicateM)
import Lib (Cell, Row, sudokuSize, getSolutions, showSudoku)

parseChar :: Char -> Cell
parseChar '.' = 0
parseChar x = digitToInt x

parseString :: String -> Row
parseString = map parseChar

readRow :: IO Row
readRow = do
  row <- parseString <$> getLine
  if length row /= sudokuSize
    then error "row does not have correct number of cells"
    else return row

main :: IO ()
main = do
    sudoku <- replicateM sudokuSize readRow
    putStrLn $ head $ showSudoku <$> getSolutions sudoku