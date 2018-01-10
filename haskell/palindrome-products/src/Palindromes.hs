module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.Maybe

isPalindromic :: Integer -> Bool
isPalindromic value = show value == (reverse . show) value

maxPalindrome :: Integer -> Integer -> (Integer, (Integer,Integer))
maxPalindrome minFactor maxFactor =
  let initialFactor =
        head [(x * y, (x,y)) |
              x <- reverse [minFactor..maxFactor],
              y <- reverse [x..maxFactor],
              isPalindromic $ x * y]
      getMaxPalindrome minFactor maxFactor factor@(palindrome, (xPrev, yPrev))
        | yNew < yPrev = palindrome
        | otherwise = getMaxPalindrome minFactor maxFactor newFactor
        where newFactor@(newPalindrome, (xNew, yNew)) =
                let newFac = listToMaybe [(x * y, (x,y)) |
                                          x <- reverse [minFactor..xPrev-1],
                                          y <- reverse [x..maxFactor],
                                          isPalindromic $ x * y]
                in if isNothing newFac
                      then (palindrome, ((-1),(-1)))
                      else fromJust newFac
      palindrome = getMaxPalindrome minFactor maxFactor initialFactor
  in head [(x * y, (x,y)) |
           x <- reverse [minFactor..maxFactor],
           y <- reverse [x..maxFactor],
           x * y == palindrome]

minPalindrome :: Integer -> Integer -> (Integer, (Integer,Integer))
minPalindrome minFactor maxFactor =
  head [(x * y, (x,y)) |
        x <- [minFactor..maxFactor],
        y <- [minFactor..x],
        isPalindromic $ x * y]

getFactor :: Integer -> Integer -> Integer ->
             (Integer, Integer) -> Maybe (Integer, Integer)
getFactor minFactor maxFactor palindrome (xMax,yMin) =
  listToMaybe [(x,y) |
               x <- reverse [minFactor..xMax-1],
               y <- [yMin..maxFactor],
               x * y == palindrome]

getFactors minFactor maxFactor palindrome factors
  | isNothing factor = factors
  | otherwise =
    getFactors minFactor maxFactor palindrome $ fromJust factor :factors
  where factor = getFactor minFactor maxFactor palindrome $ head factors

largestPalindrome :: Integer -> Integer -> (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
  let (palindrome, factor) = maxPalindrome minFactor maxFactor
  in (palindrome, getFactors minFactor maxFactor palindrome [factor])

smallestPalindrome :: Integer -> Integer -> (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
  let (palindrome, factor) = minPalindrome minFactor maxFactor
  in (palindrome, getFactors minFactor maxFactor palindrome [factor])
