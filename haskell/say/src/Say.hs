module Say (inEnglish) where

import Data.Maybe
import Data.List (intercalate)
import Data.Char

data Range = Tens | Hundreds | Thousands | Millions | Billions
  deriving (Show, Enum)

maybeNonzero :: Integer -> Maybe Int
maybeNonzero input = if (input > 0) then Just (fromInteger input) else Nothing

(<>) :: Integer -> Range -> Maybe Int
input <> Tens = maybeNonzero $ rem input 100
input <> Hundreds = maybeNonzero . flip rem 10 . flip div 100 $ input
input <> Thousands = maybeNonzero . flip rem 1000 . flip div 1000 $ input
input <> Millions = maybeNonzero . flip rem 1000 . flip div 1000000 $ input
input <> Billions = maybeNonzero . flip rem 1000 . flip div 1000000000 $ input

onesTeensStr = ["zero", "one", "two", "three", "four", "five", "six", "seven",
                "eight", "nine", "ten", "eleven", "twelve", "thirteen",
                "fourteen", "fifteen", "sixteen", "seventeen", "eighteen",
                "nineteen"]
tensStr = ["twenty", "thirty", "forty", "fifty",
           "sixty", "seventy", "eighty", "ninety"]

tens :: Integer -> Maybe String
tens inputInteger
  | isNothing input = Nothing
  | (fromJust input) < 20 = Just $ onesTeensStr !! (fromJust input)
  | onesIdx == 0 = Just $ tensStr !! tensIdx
  | otherwise = Just $ tensStr !! tensIdx ++ "-" ++ onesTeensStr !! onesIdx
  where input = inputInteger <> Tens
        tensIdx = div (fromJust input) 10 - 2
        onesIdx = mod (fromJust input) 10

hundreds :: Integer -> Maybe String
hundreds inputInteger
  | isNothing input = Nothing
  | otherwise = Just $ onesTeensStr !! (fromJust input) ++ " hundred"
  where input = inputInteger <> Hundreds

printRange :: Range -> Integer -> Maybe String
printRange range inputInteger
  | isNothing input = Nothing
  | otherwise = Just $ (intercalate " " $ catMaybes
                        [hundreds inputBignum,
                         tens inputBignum]) ++ " "
                       ++ (map toLower . init . show) range
  where input = inputInteger <> range
        inputBignum = toInteger $ fromJust input

inEnglish input
  | input < 0 = Nothing
  | input == 0 = Just "zero"
  | otherwise = Just $ (intercalate " " $
    catMaybes [printRange Billions input, printRange Millions input,
               printRange Thousands input, hundreds input, tens input])
