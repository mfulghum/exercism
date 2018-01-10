{-# LANGUAGE FlexibleInstances #-}
module SecretHandshake (handshake) where

import Data.Char
import Data.Bits

class HandshakeCode a where
  toCode :: a -> Int

instance HandshakeCode Int where
  toCode code = code
  
instance HandshakeCode String where
  toCode code =
    if all (`elem` "01") code
       then foldl (\acc x -> acc * 2 + digitToInt x) 0 code
       else 0

handshakeResponses = ["wink", "double blink", "close your eyes", "jump"]

handshake codeInput =
  let code = toCode codeInput
      response = [handshakeResponses !! codeBit | codeBit <- [0..3], testBit code codeBit]
  in if testBit code 4
        then reverse response
        else response
