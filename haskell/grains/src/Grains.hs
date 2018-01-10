module Grains (square, total) where

import Data.Maybe

square :: Integer -> Maybe Integer
square n =
  if n >= 1 && n <= 64
     then Just (2 ^ (n - 1))
     else Nothing

total :: Integer
total = sum (catMaybes [square i | i <- [1..64]])
