module Phone (areaCode, number, prettyPrint) where

import Data.Maybe
import Data.Char

areaCode :: String -> Maybe String
areaCode numString =
  let phoneNumber = number numString
  in if isJust phoneNumber
        then Just (take 3 $ fromJust phoneNumber)
        else Nothing

number :: String -> Maybe String
number numString =
  let numbers = [input | input <- numString, isDigit input]
  in if length numbers == 10
        then Just numbers
        else if length numbers == 11 && (numbers !! 0) == '1'
                then Just (drop 1 numbers)
                else Nothing

prettyPrint :: String -> Maybe String
prettyPrint numString =
  let phoneNumber = number numString
  in if isJust phoneNumber
        then Just ("(" ++ (take 3 $ fromJust phoneNumber) ++ ") " ++
                   ((take 3 . drop 3) $ fromJust phoneNumber) ++ "-" ++
                   ((take 4 . drop 6) $ fromJust phoneNumber))
        else Nothing
