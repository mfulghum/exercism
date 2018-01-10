module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

import Data.Maybe

data BST a = Nil | Node a (BST a) (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Nil = Nothing
bstLeft (Node _ left _) = Just left

bstRight :: BST a -> Maybe (BST a)
bstRight Nil = Nothing
bstRight (Node _ _ right) = Just right

bstValue :: BST a -> Maybe a
bstValue Nil = Nothing
bstValue (Node val _ _) = Just val

empty :: BST a
empty = Nil

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) Nil

insert :: Ord a => a -> BST a -> BST a
insert value Nil = singleton value
insert value (Node val left right)
  | value <= val = Node val (insert value left) right
  | value > val = Node val left (insert value right)

singleton :: a -> BST a
singleton value = Node value Nil Nil

toList :: BST a -> [a]
toList Nil = []
toList (Node val left right) =
  (toList left) ++ [val] ++ (toList right)
