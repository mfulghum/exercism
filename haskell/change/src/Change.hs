module Change (findFewestCoins) where

import Data.Ord
import Data.List
import Data.Maybe

calcDenomination input denomination =
  (mod input denomination,
   flip replicate denomination $ div input denomination)

calcCoins' target [] output = (target, output)
calcCoins' target (x:xs) output =
  let (newTarget, coinTotals) = calcDenomination target x
  in calcCoins' newTarget xs $ coinTotals ++ output

calcCoins target coins
  | remainder > 0 = Nothing
  | otherwise = Just output
  where (remainder, output) = calcCoins' target coins []

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target == 0 = Just []
  | target < minimum coins = Nothing
  | otherwise = Just . map toInteger $ minimumBy (comparing length) coinCombos
  where coinList = permutations . map fromInteger $ coins
        coinCombos = nub $ mapMaybe (calcCoins $ fromInteger target) coinList
