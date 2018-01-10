module Sieve (primesUpTo) where

primes = 2 : [i | i <- [3,5..],
              and [rem i p > 0 |
                   p <- takeWhile (\p -> p^2 <= i) primes]]

primesUpTo n = takeWhile (<= n) primes
