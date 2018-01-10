module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Nil | LinkedList { datum :: a
                                     , next :: LinkedList a
                                     } deriving (Show)
  
new :: a -> LinkedList a -> LinkedList a
new = LinkedList

fromList :: [a] -> LinkedList a
fromList [] = nil
fromList (x:xs) = new x (fromList xs)

toList :: LinkedList a -> [a]
toList Nil = []
toList (LinkedList x xs) = x : toList xs

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = fromList . reverse . toList

