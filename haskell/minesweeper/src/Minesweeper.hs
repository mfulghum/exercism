module Minesweeper (annotate) where

import Data.Array
import Data.Char (intToDigit)

boardBounds board = ((1,1), (length board, length $ head board))
boardArray board = listArray (boardBounds board) $ concat board

adjacentMines board (row,col) =
  length $ filter (=='*') [board ! (i,j) |
                           (i,j) <- indices board,
                           abs (i - row) <= 1,
                           abs (j - col) <= 1]

updateBoard board =
  board // [(idx, updateSpace board idx) | idx <- indices board]
  where updateSpace board' idx' =
          if board' ! idx' == '*'
             then '*'
             else processDigit $ adjacentMines board' idx'
                  where processDigit 0 = ' '
                        processDigit n = intToDigit n

getRow board row =
  [x | ((i,j), x) <- assocs board, i == row]

annotate :: [String] -> [String]
annotate board =
  let board' = boardArray board
      annotatedArray = updateBoard board'
  in [getRow annotatedArray row | row <- [1..length board]]
