module Scrabble (scoreLetter, scoreWord) where

import Data.Char

letterScores = [("AEIOULNRST", 1),
                ("DG", 2),
                ("BCMP", 3),
                ("FHVWY", 4),
                ("K", 5),
                ("JX", 8),
                ("QZ", 10)]

scoreLetter :: Char -> Integer
scoreLetter letter =
  let result = filter (elem (toUpper letter) . fst) letterScores
  in case result of
    [] -> 0
    _ -> (snd . head) result
  
scoreWord :: String -> Integer
scoreWord word = sum $ map scoreLetter word
