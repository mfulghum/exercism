module Queens (boardString, canAttack) where

import Data.Maybe
import Data.List (intercalate)

type Queen = Maybe (Int, Int)

boardChar :: Queen -> Queen -> (Int, Int) -> Char
boardChar Nothing Nothing _ = '_'
boardChar w b pos
  | isJust w && (fromJust w == pos) = 'W'
  | isJust b && (fromJust b == pos) = 'B'
  | otherwise = '_'


boardRank :: Queen -> Queen -> Int -> String
boardRank white black y =
  intercalate " " [[boardChar white black (y,x)] | x <- [0..7]]

boardString :: Queen -> Queen -> String
boardString white black =
  unlines [boardRank white black y | y <- [0..7]]

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (y0,x0) (y1,x1)
  | (y1 == y0) || (x1 == x0) = True
  | abs (y1 - y0) == abs (x1 - x0) = True
  | otherwise = False
