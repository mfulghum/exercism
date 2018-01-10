{-# LANGUAGE BangPatterns #-}

module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' op defaultValue [] = defaultValue
foldl' op !defaultValue (x:xs) = foldl' op (op defaultValue x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op defaultValue [] = defaultValue
foldr op defaultValue (x:xs) = op x (foldr op defaultValue xs)

length :: [a] -> Int
length = foldl' (\accum _ -> accum + 1) 0

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) [] 

map :: (a -> b) -> [a] -> [b]
map op = foldr ((:) . op) []

filter :: (a -> Bool) -> [a] -> [a]
filter op = foldr (\x xs -> if op x then (x:xs) else xs) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []
