module Beer (song) where

import Data.Char
import Data.List

capitalized :: String -> String
capitalized (x:xs) = toUpper x : xs
capitalized [] = []

bottles :: Integer -> String
bottles 1 = "1 bottle"
bottles 0 = "no more bottles"
bottles (-1) = bottles 99
bottles numBottles = show numBottles ++ " bottles"

takeBottle :: Integer -> String
takeBottle 1 = "Take it down and pass it around"
takeBottle 0 = "Go to the store and buy some more"
takeBottle _ = "Take one down and pass it around"

verse :: Integer -> String
verse numBottles =
  (capitalized . bottles) numBottles ++
  " of beer on the wall, " ++
  bottles numBottles ++
  " of beer.\n" ++
  takeBottle numBottles ++
  ", " ++
  bottles (numBottles - 1) ++
  " of beer on the wall.\n"

song :: String
song = intercalate "\n" [verse numBottles | numBottles <- [99,98..0]]
