module RunLength (decode, encode) where

import Data.Char

decode :: String -> String
decode input =
  decode' input []
  where decode' [] output = output
        decode' input output =
          let numberString = takeWhile isDigit input
              number = if null numberString
                          then 1
                          else read numberString::Int
              x = head $ drop (length numberString) input
          in decode' (drop (length numberString + 1) input) $
             output ++ replicate number x

encode :: String -> String
encode input =
  encode' input []
  where encode' [] output = output
        encode' input@(x:xs) output =
          let numCharsEqual = length $ takeWhile (==x) input
              printNumChars 1 = ""
              printNumChars n = show n
          in encode' (drop numCharsEqual input) $
             output ++ printNumChars numCharsEqual ++ [x]
