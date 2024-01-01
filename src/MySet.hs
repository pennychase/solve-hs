module MySet where

data MySet a = MySet (TreeNode a) Int
    deriving (Show, Eq)

data TreeNode a = Node a (TreeNode a) (TreeNode a) | Nil
    deriving (Show, Eq)

empty :: MySet a
empty = MySet Nil 0

singleton :: (Ord a) => a -> MySet a
singleton a = MySet (Node a Nil Nil) 1

fromList :: (Ord a) => [a] -> MySet a
fromList = foldr insert empty

toList :: (Ord a) => MySet a -> [a]
toList (MySet Nil _) = []
toList (MySet tr _) = treeToList tr
    where
        treeToList :: (Ord a) => TreeNode a -> [a]
        treeToList Nil = []
        treeToList (Node v l r) = treeToList l <> [v] <> treeToList r

null :: MySet a -> Bool
null (MySet Nil _) = True
null _ = False

size :: MySet a -> Int
size (MySet _ n) = n

insert :: (Ord a) => a -> MySet a -> MySet a
insert a st =
    case st of
        MySet Nil _ -> singleton a
        MySet tr n -> 
            let (i, tr') = insertTree a tr
            in MySet tr' (n + i)
    where
        insertTree :: (Eq a, Ord a) => a -> TreeNode a -> (Int, TreeNode a)
        insertTree a Nil = (1, Node a Nil Nil)
        insertTree a tr@(Node v l r)
            | a == v = (0, tr)
            | a < v = 
                let (i, l') = insertTree a l
                in (i, Node v l' r)
            | a > v =
                let (i, r') = insertTree a r
                in (i, Node v l r')

-- Deletion cases
-- Node has no children - delete node
-- Node has one child - child node replace the node
-- Node has two children - replace node with the smallest node in the right child
delete :: (Ord a) => a -> MySet a -> MySet a
delete a st = 
    case st of
        MySet Nil 0 -> MySet Nil 0
        MySet tr n -> 
            let (i, tr') = deleteTree a tr
            in MySet tr' (n - i)

deleteTree :: (Eq a, Ord a) => a -> TreeNode a -> (Int, TreeNode a)
deleteTree a Nil = (0, Nil)
deleteTree a tr@(Node v l r)
    | a == v = (1, deleteNode tr)
    | a < v = 
        let (i, l') = deleteTree a l
        in (i, Node v l' r)
    | a > v =
        let (i, r') = deleteTree a r
        in (i, Node v l r')  
    where
        deleteNode :: (Eq a, Ord a) => TreeNode a -> TreeNode a   
        deleteNode tr =
            case tr of
                Node _ Nil Nil -> Nil
                Node _ l Nil -> l
                Node _ Nil r -> r
                Node _ l r -> 
                    let (v', r') = updateNode r 
                    in Node v' l r'
        updateNode :: (Eq a, Ord a) => TreeNode a -> (a, TreeNode a)
        updateNode (Node val left right) = go left
            where
                go Nil = (val, Nil)
                go (Node v' Nil Nil) = (v', Nil)
                go (Node v l r) = 
                    let (v', l') = go l
                    in (v', Node v l' r)


member :: (Ord a) => a -> MySet a -> Bool
member a (MySet Nil _) = False
member a (MySet tr _) = memberTree a tr
    where
        memberTree :: (Eq a, Ord a) => a -> TreeNode a -> Bool
        memberTree a Nil = False
        memberTree a (Node v l r) 
            | a == v = True
            | a < v = memberTree a l
            | a > v = memberTree a r
