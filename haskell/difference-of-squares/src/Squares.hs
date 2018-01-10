module Squares (difference, squareOfSums, sumOfSquares) where

difference :: Integral a => a -> a
difference x =
  squareOfSums x - sumOfSquares x

squareOfSums :: Integral a => a -> a
squareOfSums x =
  sum [0..x] ^ 2

sumOfSquares :: Integral a => a -> a
sumOfSquares x =
  sum [i*i | i <- [0..x]]
