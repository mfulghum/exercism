module Strain (keep, discard) where

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep op (x:xs) =
  if op x
     then x : keep op xs
     else keep op xs

discard :: (a -> Bool) -> [a] -> [a]
discard op inputList = keep (not . op) inputList
