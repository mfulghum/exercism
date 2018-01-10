module Prime (nth) where

import Data.Maybe

primes = 2 : [i | i <- [3,5..],
              and [rem i p > 0 |
                   p <- takeWhile (\p -> p^2 <= i) primes]]

nth n
  | n == 0 = Nothing
  | otherwise = Just (primes !! (n - 1))
