module Luhn (addends, checkDigit, checksum, create, isValid) where

import Data.Char (digitToInt, intToDigit)

doubleDigits [] = []
doubleDigits (x:xs) =
  if (odd $ length xs)
     then (if x*2 >= 10 then x*2 - 9 else x*2) : doubleDigits xs
     else x : doubleDigits xs

addends = doubleDigits . map digitToInt . (show :: Integer -> String)

checkDigit = digitToInt . last . (show :: Integer -> String)

checksum = flip mod 10 . sum . addends

create input =
  let readInt = read :: String -> Integer
      showInt = show :: Integer -> String
  in readInt $ (showInt input) ++
               [intToDigit . flip mod 10 $ (10 - checksum (input * 10))]

isValid = (==0) . flip mod 10 . sum . addends
