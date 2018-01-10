module Atbash (decode, encode) where

import Data.Maybe
import Data.Char

cipher :: Char -> Maybe Char
cipher input
  | input == ' ' = Nothing
  | (not . isAscii) input = Nothing
  | elem (toLower input) ['a'..'z'] =
    Just (chr $ ord 'z' + ord 'a' - ord (toLower input))
  | elem input ['0'..'9'] = Just input
  | otherwise = Nothing

splitString :: String -> String -> String
splitString output input
  | length input <= 5 = output ++ input
  | otherwise = splitString (output ++ take 5 input ++ " ") (drop 5 input)
  
decode :: String -> String
decode = mapMaybe cipher

encode :: String -> String
encode = splitString [] . mapMaybe cipher 
