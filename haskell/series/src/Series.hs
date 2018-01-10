module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n xs
  | n == 0 = [[]]
  | n > (length xs) = []
  | otherwise = let digits = map digitToInt xs
                in [take n $ drop m digits | m <- [0..length digits - n]]
