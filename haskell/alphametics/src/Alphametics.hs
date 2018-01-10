module Alphametics (solve) where

import Data.List
import Data.Char (intToDigit, isLetter)
import Data.Maybe

getNumbers n
  | n == 1 = [[x] | x <- [0..9]]
  | otherwise = concat [[x : xs |
                         x <- [0..9], notElem x xs] |
                         xs <- getNumbers $ n - 1]

getLetters = nub . filter isLetter

getPermutations input =
  let letters = getLetters input
      numbers = getNumbers $ length letters
  in map (zip letters) numbers

replace :: [(Char, Int)] -> Char -> Char
replace charMap input
  | null replace' = input
  | otherwise = head replace'
  where replace' = [intToDigit b | (a,b) <- charMap, input == a]

readIntMaybe :: String -> Maybe Integer
readIntMaybe str =
  case reads str :: [(Integer, String)] of
    [(x, _)] -> Just x
    _ -> Nothing

checkValid input charMap
  | elem '0' . map head . words $ inputMap = False
  | otherwise = (sum $ init numList) == last numList
  where inputMap = map (replace charMap) input
        numList = mapMaybe readIntMaybe $ words inputMap

solve :: String -> Maybe [(Char, Int)]
solve puzzle
  | null solutions = Nothing
  | otherwise = Just $ head solutions
  where solutions = filter (checkValid puzzle) $ getPermutations puzzle
