module Roman (numerals) where

iterateNumber :: Integer -> String -> String
iterateNumber 0 outStr = outStr
iterateNumber val inStr
  | divisibleBy 1000 = iterateNumber (val - 1000) (inStr ++ "M")
  | divisibleBy 900 = iterateNumber (val - 900) (inStr ++ "CM")
  | divisibleBy 500 = iterateNumber (val - 500) (inStr ++ "D")
  | divisibleBy 400 = iterateNumber (val - 400) (inStr ++ "CD")
  | divisibleBy 100 = iterateNumber (val - 100) (inStr ++ "C")
  | divisibleBy 90 = iterateNumber (val - 90) (inStr ++ "XC")
  | divisibleBy 50 = iterateNumber (val - 50) (inStr ++ "L")
  | divisibleBy 40 = iterateNumber (val - 40) (inStr ++ "XL")
  | divisibleBy 10 = iterateNumber (val - 10) (inStr ++ "X")
  | divisibleBy 9 = iterateNumber (val - 9) (inStr ++ "IX")
  | divisibleBy 5 = iterateNumber (val - 5) (inStr ++ "V")
  | divisibleBy 4 = iterateNumber (val - 4) (inStr ++ "IV")
  | otherwise = iterateNumber (val - 1) (inStr ++ "I")
  where divisibleBy = (/= 0) . (div val)

numerals :: Integer -> Maybe String
numerals number =
  if number <= 0
     then Nothing
     else Just (iterateNumber number "")
