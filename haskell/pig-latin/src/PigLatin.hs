module PigLatin (translate) where

import Data.List (intercalate)

vowels = ['a','e','i','o','u']
vowelExceptions = ["yt", "xr"]

consonants = [char | char <- ['a'..'z'], notElem char vowels]
threeLetterClusters = ["thr", "sch"] ++ [char : "qu" | char <- consonants]
twoLetterClusters = ["ch", "qu", "th"]

translateWord :: String -> String
translateWord input
  | flip elem vowels $ head input = input ++ "ay"
  | flip elem vowelExceptions $ take 2 input = input ++ "ay"
  | flip elem threeLetterClusters $ take 3 input =
    drop 3 input ++ take 3 input ++ "ay"
  | flip elem twoLetterClusters $ take 2 input =
    drop 2 input ++ take 2 input ++ "ay"
  | otherwise = drop 1 input ++ take 1 input ++ "ay"

translate :: String -> String
translate = intercalate " " . map translateWord . words
