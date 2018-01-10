module Series (largestProduct) where

import Data.Char (digitToInt, isDigit)

sampleDigits :: Int -> String -> [String]
sampleDigits numDigits digits =
  [take numDigits $ drop n digits | n <- [0..length digits - numDigits]]

largestProduct :: Int -> String -> Maybe Int
largestProduct 0 _ = Just 1
largestProduct numDigits digits =
  if numDigits > 0 && numDigits <= length digits && all isDigit digits
     then Just (maximum [product $ map digitToInt sample | sample <- sampleDigits numDigits digits])
     else Nothing
