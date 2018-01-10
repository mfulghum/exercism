module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Control.Monad (liftM, liftM2)
import System.Random (randomRs, newStdGen)
import Data.Char

decodeLetter cipher character =
  let shift = (flip (-) $ ord 'a') . ord $ cipher
  in chr .
     ((+) $ ord 'a') .
     (flip mod 26) .
     (flip (-) shift) .
     (flip (-) $ ord 'a') .
     ord .
     toLower $
     character

encodeLetter cipher character =
  let shift = (flip (-) $ ord 'a') . ord $ cipher
  in chr .
     ((+) $ ord 'a') .
     (flip mod 26) .
     (flip (+) shift) .
     (flip (-) $ ord 'a') .
     ord .
     toLower $
     character

caesarDecode :: String -> String -> String
caesarDecode cipherRaw input
  | length cipher == length input =
    map (\(c,i) -> decodeLetter c i) $ zip cipher input
  | otherwise = map (decodeLetter $ head cipher) input
  where cipher = take (length input) cipherRaw

caesarEncode :: String -> String -> String
caesarEncode cipherRaw input
  | length cipher == length input =
    map (\(c,i) -> encodeLetter c i) $ zip cipher input
  | otherwise = map (encodeLetter $ head cipher) input
  where cipher = take (length input) cipherRaw

randomCipher :: IO String
randomCipher = liftM (take 100 . randomRs('a','z')) newStdGen

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom input = do
  cipher <- randomCipher
  let encoded = caesarEncode cipher input
  return (cipher, encoded)
