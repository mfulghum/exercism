module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor input
  | silentTreatment input = "Fine. Be that way!"
  | isShouting input = "Whoa, chill out!"
  | isQuestion input = "Sure."
  | otherwise = "Whatever."

silentTreatment :: String -> Bool
silentTreatment input = all isSpace input

isQuestion :: String -> Bool
isQuestion input = head (dropWhile isSpace (reverse input)) == '?'

isShouting :: String -> Bool
isShouting input = any isUpper input && not (any isLower input)
