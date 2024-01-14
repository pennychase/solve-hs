{-# OPTIONS_GHC -fno-warn-partial-fields #-}

module BalancedSet where

data MySet a = MySet (TreeNode a) Int
  deriving (Show, Eq)

-- Node a left right balance hight
data TreeNode a = Node a (TreeNode a) (TreeNode a) Int Int| Nil
  deriving (Show, Eq)

empty :: MySet a
empty = MySet Nil 0

singleton :: (Ord a) => a -> MySet a
singleton a = MySet (Node a Nil Nil 0 1) 1

fromList :: (Ord a) => [a] -> MySet a
fromList = foldr insert empty

toList :: (Ord a) => MySet a -> [a]
toList (MySet Nil _) = []
toList (MySet tr _) = treeToList tr
    where
        treeToList :: (Ord a) => TreeNode a -> [a]
        treeToList Nil = []
        treeToList (Node v l r _ _) = treeToList l <> [v] <> treeToList r

null :: MySet a -> Bool
null (MySet Nil _) = True
null _ = False

size :: MySet a -> Int
size (MySet _ s) = s

member :: (Ord a) => a -> MySet a -> Bool
member a (MySet Nil _) = False
member a (MySet tr _) = memberTree a tr
  where
    memberTree :: (Ord a, Eq a) => a -> TreeNode a -> Bool
    memberTree a Nil = False
    memberTree a (Node v l r _ _)
      | a == v = True
      | a < v = memberTree a l
      | a > v = memberTree a r

nodeHeight :: TreeNode a -> Int
nodeHeight Nil = 0
nodeHeight (Node _ _ _ _ h) = h

nodeVal :: TreeNode a -> a
nodeVal Nil = error "calloing nodeVal on empty tree"
nodeVal (Node v _ _ _ _) = v

nodeBalance :: TreeNode a -> Int
nodeBalance Nil = 0
nodeBalance (Node _ _ _ b _) = b

leftChild :: TreeNode a -> TreeNode a
leftChild Nil = Nil
leftChild (Node _ l _ _ _) = l

rightChild :: TreeNode a -> TreeNode a
rightChild Nil = Nil
rightChild (Node _ _ r _ _) = r

balanceNode :: TreeNode a -> TreeNode a
balanceNode Nil = Nil
balanceNode (Node v l r b h) = Node v l r b' h'
  where
    h' = 1 + max (nodeHeight l) (nodeHeight r)
    b' = nodeHeight l - nodeHeight r

findMinVal :: TreeNode a -> a
findMinVal Nil = error "Calling findMinVal on empty tree"
findMinVal (Node v Nil _ _ _) = v
findMinVal (Node _ l _ _ _) = findMinVal l

rotateLeft :: TreeNode a -> TreeNode a
rotateLeft Nil = Nil
rotateLeft node@(Node _ _ Nil _ _) = node
rotateLeft (Node v l r b h) = balanceNode (Node (nodeVal r) newLeft (rightChild r) b h)
  where
    newLeft = balanceNode (Node v l (leftChild r) (nodeBalance l) (nodeBalance r))

rotateRight :: TreeNode a -> TreeNode a
rotateRight Nil = Nil
rotateRight node@(Node _ Nil _ _ _) = node
rotateRight (Node v l r b h) = balanceNode (Node (nodeVal l) (leftChild l) newRight b h)
  where
    newRight = balanceNode (Node v (rightChild l) r (nodeBalance r) (nodeHeight r))

rebalanceTree :: TreeNode a -> TreeNode a
rebalanceTree Nil = Nil
rebalanceTree tr@(Node v l r b h)
  | balanceFactor < (-1) =
      if bfRightChild <= 0 then rotateLeft tr else rotateLeft (Node v l (rotateRight (rightChild tr)) b h)
  | balanceFactor > 1 =
      if bfLeftChild >= 0 then rotateRight tr else rotateRight (Node v (rotateLeft (leftChild tr)) r b h)
  | otherwise = tr
  where
    balanceFactor = nodeBalance tr
    bfRightChild = nodeBalance (rightChild tr)
    bfLeftChild = nodeBalance (leftChild tr)

insert :: (Ord a) => a -> MySet a -> MySet a
insert a st@(MySet root n) =
  if member a st
    then st
    else MySet (insertNode  a root) (n + 1)

insertNode :: (Ord a) => a -> TreeNode a -> TreeNode a
insertNode a Nil = Node a Nil Nil 0 1
insertNode a node@(Node v l r b h) = rebalanceTree . balanceNode $ node'
  where
    node' = case compare a v of
      EQ -> node
      LT -> Node v (insertNode a l) r b h
      GT -> Node v l (insertNode a r) b h

delete :: (Ord a) => a -> MySet a -> MySet a
delete a (MySet Nil _) = MySet Nil 0
delete a (MySet root n) = MySet (deleteNode a root) (n - 1)

deleteNode :: (Ord a) => a -> TreeNode a -> TreeNode a
deleteNode _ Nil = Nil
deleteNode a node@(Node v l r b h) = rebalanceTree . balanceNode $ node'
  where
    node' = case compare a v of
      EQ -> deleteHelper node
      LT -> Node v (deleteNode a l) r b h
      GT -> Node v l (deleteNode a r) b h
    deleteHelper (Node v Nil r b h) = r
    deleteHelper (Node v l Nil b h) = l
    deleteHelper (Node v l r b h) = 
      let
        v' = findMinVal r
      in (Node v' l (deleteNode v' r) b h)
