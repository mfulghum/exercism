module Accumulate (accumulate) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate _ [] = []
accumulate op (x:xs) = op x : accumulate op xs
