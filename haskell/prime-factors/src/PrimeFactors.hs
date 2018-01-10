module PrimeFactors (primeFactors) where

primes = 2 : [i | i <- [3,5..], and [rem i p > 0 |  p <- takeWhile (\p -> p^2 <= i) primes]]

primeAccum :: Integer -> [Integer] -> [Integer]
primeAccum 1 outList = outList
primeAccum n outList =
  let factor = head [prime | prime <- primes, rem n prime == 0]
  in primeAccum (div n factor) (outList ++ [factor])

primeFactors :: Integer -> [Integer]
primeFactors num = primeAccum num []
