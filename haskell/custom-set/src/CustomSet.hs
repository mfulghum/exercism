module CustomSet
  ( delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
  ) where

import Prelude hiding (null)

data Color = Red | Black
  deriving (Eq, Show)

data CustomSet a = Nil | Node a Color (CustomSet a) (CustomSet a)
  deriving (Show)

instance Eq a => Eq (CustomSet a) where
  x == y = toList x == toList y
  x /= y = toList x /= toList y

insert :: (Ord a) => a -> CustomSet a -> CustomSet a
insert value node
  | member value node = node
  | otherwise = treeInsert node value

empty :: CustomSet a
empty = Nil

null :: CustomSet a -> Bool
null Nil = True
null _ = False

fromList :: (Ord a) => [a] -> CustomSet a
fromList [] = Nil
fromList input = foldl (flip insert) Nil input

toList :: CustomSet a -> [a]
toList Nil = []
toList (Node value _ left right) =
  (toList left) ++ [value] ++ (toList right)

delete :: (Ord a) => a -> CustomSet a -> CustomSet a
delete value node = fromList . filter (/= value) . toList $ node

member :: (Ord a) => a -> CustomSet a -> Bool
member value Nil = False
member value (Node nodeVal _ left right)
  | value < nodeVal = member value left
  | value > nodeVal = member value right
  | otherwise = True

size :: CustomSet a -> Int
size Nil = 0
size (Node _ _ left right) =
  (size left) + 1 + (size right)

isDisjointFrom :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom _ Nil = True
isDisjointFrom Nil _ = True
isDisjointFrom x@(Node val _ Nil Nil) y =
  not $ member val y
isDisjointFrom x@(Node val _ left Nil) y =
  not $ member val y || isSubsetOf left y
isDisjointFrom x@(Node val _ Nil right) y =
  not $ member val y || isSubsetOf right y
isDisjointFrom x@(Node val _ left right) y =
  not $ member val y || isSubsetOf left y || isSubsetOf right y

isSubsetOf :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf Nil _ = True
isSubsetOf _ Nil = False
isSubsetOf x@(Node val _ Nil Nil) y =  member val y
isSubsetOf x@(Node val _ left Nil) y =  member val y && isSubsetOf left y
isSubsetOf x@(Node val _ Nil right) y =  member val y && isSubsetOf right y
isSubsetOf x@(Node val _ left right) y =  member val y && isSubsetOf left y && isSubsetOf right y

union :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union Nil y = y
union x Nil = x
union x y = foldl (flip insert) x (toList y)

difference :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
difference Nil _ = Nil
difference x Nil = x
difference x y = foldl (flip delete) x $ toList y

intersection :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
intersection Nil _ = Nil
intersection _ Nil = Nil
intersection x y = foldl (flip insert) Nil $ filter (flip member x) (toList y)

{--------------------------------------------------
Red-Black Tree definition follows...
(Built following this article on implementing red-black trees in Haskell:
http://scienceblogs.com/goodmath/2009/11/30/advanced-haskell-data-structur/)
--------------------------------------------------}

treeLeft :: (Ord a) => CustomSet a -> CustomSet a
treeLeft (Node _ _ left _) = left

treeRight :: (Ord a) => CustomSet a -> CustomSet a
treeRight (Node _ _ _ right) = right

treeValue :: (Ord a) => CustomSet a -> a
treeValue (Node value _ _ _) = value

treeColor :: (Ord a) => CustomSet a -> Color
treeColor (Node _ color _ _) = color
treeColor Nil = Black

treeInsert node val =
  treeInsertTailCall node val []

treeInsertTailCall node@(Node nodeVal _ left right) val path
  | val < nodeVal = treeInsertTailCall left val (node:path)
  | otherwise = treeInsertTailCall right val (node:path)
treeInsertTailCall Nil val path =
  treeRebalance (Node val Red Nil Nil) path

treeReconstructNode node@(Node val _ _ _) parent@(Node parentVal _ left right) color
  | val < parentVal = Node parentVal color node right
  | otherwise = Node parentVal color left node

treeRebalance :: (Ord a) => CustomSet a -> [CustomSet a] -> CustomSet a
treeRebalance (Node val _ left right) [] = Node val Black left right

treeRebalance node@(Node val _ _ _) (parent@(Node parentVal color left right):[])
  | val < parentVal = Node parentVal color node right
  | otherwise = Node parentVal color left node
  
treeRebalance focus@(Node val Black _ _) (parent@(Node parentVal color left right):ancestors)
  | val < parentVal = treeRebalance (Node parentVal color focus right) ancestors
  | otherwise = treeRebalance (Node parentVal color left focus) ancestors
  
treeRebalance focus@(Node _ Red _ _) (parent@(Node _ Black _ _):ancestors) =
  treeRebalance (treeReconstructNode focus parent Black) ancestors
  
treeRebalance focus@(Node _ Red _ _) (parent@(Node _ Red _ _):ancestors) =
  treeRebalanceRedRedNode focus parent ancestors

uncleColor node parent grandparent@(Node _ _ left right) =
  if parent == left
     then treeColor right
     else treeColor left
     

data TwoStepPath = LeftLeft | LeftRight | RightLeft | RightRight

pathFromGrandparent :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a -> TwoStepPath
pathFromGrandparent node@(Node val _ _ _) parent@(Node parentVal _ _ _) grandparent@(Node grandparentVal _ _ _)
  | parentVal < grandparentVal && val < parentVal = LeftLeft
  | parentVal >= grandparentVal && val < parentVal = RightLeft
  | parentVal < grandparentVal && val >= parentVal = LeftRight
  | parentVal >= grandparentVal && val >= parentVal = RightRight

treeRebalanceRedRedNode focus parent [] =
  treeRebalance (treeReconstructNode focus parent Red) []

treeRebalanceRedRedNode focus parent (grand:ancestors)
  | (uncleColor focus parent grand) == Red = recolorAndContinue focus parent grand ancestors
  | otherwise = case (pathFromGrandparent focus parent grand) of
                  LeftLeft -> treeRebalance (pivotGrandparentRight focus parent grand) ancestors
                  LeftRight -> treeRebalance (pivotParentLeft focus parent) (grand:ancestors)
                  RightLeft -> treeRebalance (pivotParentRight focus parent) (grand:ancestors)
                  RightRight -> treeRebalance (pivotGrandparentLeft focus parent grand) ancestors

treePivotLeft :: (Ord a) => CustomSet a -> Bool -> CustomSet a
treePivotLeft (Node rootVal rootColor sibling (Node focusVal focusColor focusLeft focusRight)) swap
  | swap = Node focusVal rootColor (Node rootVal focusColor sibling focusLeft) focusRight
  | otherwise = Node focusVal focusColor (Node rootVal rootColor sibling focusLeft) focusRight

treePivotRight :: (Ord a) => CustomSet a -> Bool -> CustomSet a
treePivotRight (Node rootVal rootColor (Node focusVal focusColor focusLeft focusRight) sibling) swap
  | swap = Node focusVal rootColor focusLeft (Node rootVal focusColor focusRight sibling)
  | otherwise = Node focusVal focusColor focusLeft (Node rootVal rootColor focusRight sibling)

pivotParentLeft node parent@(Node val color left right) =
  treePivotLeft (Node val color left node) False

pivotParentRight node parent@(Node val color left right) =
  treePivotRight (Node val color node right) False

pivotGrandparentLeft node parent@(Node val color left right) grandparent@(Node grandparentVal grandparentColor grandparentLeft grandparentRight) =
  treePivotLeft (Node grandparentVal grandparentColor grandparentLeft (Node val color left node)) True

pivotGrandparentRight node parent@(Node val color left right) grandparent@(Node grandparentVal grandparentColor grandparentLeft grandparentRight) =
  treePivotRight (Node grandparentVal grandparentColor (Node val color node right) grandparentRight) True

recolorAndContinue focus parent grand@(Node value color left right) ancestors =
  let path = pathFromGrandparent focus parent grand
      uncle = (case path of
                 LeftLeft -> right
                 LeftRight -> right
                 RightLeft -> left
                 RightRight -> left)
      newUncle = if null uncle
                    then Nil
                    else Node (treeValue uncle) Black (treeLeft uncle) (treeRight uncle)
      newParent = treeReconstructNode focus parent Black
      newGrandparent = (case path of
                          LeftLeft -> Node value Red newParent newUncle
                          LeftRight -> Node value Red newParent newUncle
                          RightLeft -> Node value Red newUncle newParent
                          RightRight -> Node value Red newUncle newParent)
  in treeRebalance newGrandparent ancestors
