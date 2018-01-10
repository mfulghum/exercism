module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

import Data.List (sort)

isPythagorean :: [Integer] -> Bool
isPythagorean tri =
  let triplet = map (^2) . sort $ tri
  in (sum $ init triplet) == last triplet

mkTriplet a b c = a : b : c : []

pythagoreanTriplets from to =
  sort [[a,b,c] |
        c <- [from..to], a <- [from..c-1], b <- [a..c-1],
        isPythagorean $ mkTriplet a b c]
