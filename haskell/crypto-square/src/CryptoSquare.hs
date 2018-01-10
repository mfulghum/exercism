module CryptoSquare (encode) where

import Data.Char (toLower)
import Data.List (intercalate, transpose)

normalize :: String -> String
normalize = filter (flip elem $ ['a'..'z'] ++ ['0'..'9']) . map toLower

codeShape :: String -> (Int, Int)
codeShape code =
  let sideLength = (sqrt . fromIntegral . length) code
      shapeDiv a b = (fromIntegral a) / (fromIntegral b)
  in (ceiling $ shapeDiv (length code) (ceiling sideLength),
      fromIntegral . ceiling $ sideLength)

encode :: String -> String
encode input =
  let normalized = normalize input
      shape = codeShape normalized
  in intercalate " " . transpose $
     [take (snd shape) $ drop ((snd shape) * j) normalized |
      j <- [0..fst shape - 1]]
