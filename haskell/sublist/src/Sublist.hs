module Sublist (Sublist(..), sublist) where

data Sublist = Equal | Sublist | Superlist | Unequal deriving (Eq, Show)

sublist xs ys
  | length xs == length ys && xs == ys = Equal
  | null xs || (length xs < length ys && isSublist xs ys) = Sublist
  | null ys || (length ys < length xs && isSublist ys xs) = Superlist
  | otherwise = Unequal
  where isSublist left@(x:_) right
          | null rightChunk || length rightChunk < length left = False
          | take (length left) rightChunk == left = True
          | otherwise = isSublist left $ drop 1 rightChunk
          where rightChunk = dropWhile (/= x) right 
  
