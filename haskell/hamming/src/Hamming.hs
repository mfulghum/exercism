module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance strand1 strand2 =
  if length strand1 == length strand2
     then Just (sum $ map fromEnum (zipWith (/=) strand1 strand2))
     else Nothing
