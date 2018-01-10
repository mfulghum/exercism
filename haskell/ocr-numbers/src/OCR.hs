module OCR (convert) where

import Data.List (transpose, intercalate)
import Data.Char

digitChar :: Integer -> [String]
digitChar 0 = [" _ "
              ,"| |"
              ,"|_|"
              ,"   "]
digitChar 1 = ["   "
              ,"  |"
              ,"  |"
              ,"   "]
digitChar 2 = [" _ "
              ," _|"
              ,"|_ "
              ,"   "]
digitChar 3 = [" _ "
              ," _|"
              ," _|"
              ,"   "]
digitChar 4 = ["   "
              ,"|_|"
              ,"  |"
              ,"   "]
digitChar 5 = [" _ "
              ,"|_ "
              ," _|"
              ,"   "]
digitChar 6 = [" _ "
              ,"|_ "
              ,"|_|"
              ,"   "]
digitChar 7 = [" _ "
              ,"  |"
              ,"  |"
              ,"   "]
digitChar 8 = [" _ "
              ,"|_|"
              ,"|_|"
              ,"   "]
digitChar 9 = [" _ "
              ,"|_|"
              ," _|"
              ,"   "]

digitMatch :: [Integer] -> Char
digitMatch [] = '?'
digitMatch [x] = intToDigit $ fromInteger x

convertDigit :: [String] -> Char
convertDigit char = digitMatch [x | x <- [0..9], digitChar x == char]

inTxt = unlines [ "       _     _        _  _ "
                , "  |  || |  || |  |  || || |"
                , "  |  ||_|  ||_|  |  ||_||_|"
                , "                           " ]

--convert :: String -> String
convert input =
  let inputText = lines input
      numRows = flip div 4 $ length inputText
      numCols = flip div 3 $ (length . transpose) inputText
      getDigit n = convertDigit . transpose .
        take 3 . drop (n * 3) . transpose
  in intercalate "," [[getDigit n . take 4 . drop (4*m) $ inputText |
                       n <- [0..numCols - 1]] |
                       m <- [0..numRows - 1]]
