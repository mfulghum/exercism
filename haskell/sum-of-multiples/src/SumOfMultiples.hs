module SumOfMultiples (sumOfMultiples) where

import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum (nub [i | i <- [0..limit-1], j <- factors, mod i j == 0])
