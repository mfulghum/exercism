module WordCount (wordCount) where

import Data.Char
import Data.List (nub)

stripQuotes input =
  if head input == '\''
     then take (length input - 2) $ drop 1 input
     else input
  

stripString :: String -> String
stripString [] = []
stripString (x:xs)
  | elem x validChars = x : stripString xs
  | otherwise = ' ' : stripString xs
  where validChars = ['a'..'z'] ++ ['0'..'9'] ++ " '"
  
getWords = map stripQuotes . words . stripString . map toLower

wordCount :: String -> [(String, Int)]
wordCount input =
  let words = getWords input
  in [(word, length $ filter (== word) words) | word <- nub words]
