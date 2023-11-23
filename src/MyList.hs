{-# LANGUAGE NoImplicitPrelude #-}

module MyList where

import Prelude (Bool(..), error, Int, (-), (+), (*), (&&), (||), Ord(..), ($), Maybe(..), Eq(..), Ordering(..), Monad(..), (<$>), Show(..), not, undefined)

data MyList a = 
  Nil |
  Cons a (MyList a)
  deriving (Show, Eq)

-- Module 1 Lecture 1
-- Pattern Matching
null' :: MyList a -> Bool
null' Nil = True
null' _ = False

head' :: MyList a -> a
head' (Cons x _) = x

tail' :: MyList a -> MyList a
tail' (Cons _ xs) = xs

cons' :: a -> MyList a -> MyList a
cons' x xs = Cons x xs

-- Module 1 Lecture 2
-- Basic Recursion
atIndex :: MyList a -> Int -> a
atIndex Nil _ = error "No atIndex on empty list"
atIndex (Cons x xs) n =
  if n == 0
    then x
    else atIndex xs (n - 1)

last' :: MyList a -> a
last' Nil = error "No last on empty list"
last' (Cons x Nil) = x
last' (Cons x xs) = last' xs

-- Description in exercises is wrong
-- We're looking for the first element that satisfies the predicate.
find' :: (a -> Bool) -> MyList a -> Maybe a
find' _ Nil = Nothing
find' pred (Cons x xs) = 
  if pred x
    then Just x
    else find' pred xs

elem' :: (Eq a) => a -> MyList a -> Bool
elem' _ Nil = False
elem' y (Cons x xs) = (x == y) || elem' y xs

and' :: MyList Bool -> Bool
and' Nil = True
and' (Cons x xs) = 
  if x
    then and' xs
    else False

or' :: MyList Bool -> Bool
or' Nil = False
or' (Cons x xs) = x || or' xs

any' :: (a -> Bool) -> MyList a -> Bool
any' _ Nil = False
any' pred (Cons x xs) = pred x || any' pred xs

all' :: (a -> Bool) -> MyList a -> Bool
all' _ Nil = True
all' pred (Cons x xs) = pred x && all' pred xs

isPrefixOf' :: (Eq a) => MyList a -> MyList a -> Bool
isPrefixOf' Nil _ = True
isPrefixOf' (Cons _ _) Nil = False
isPrefixOf' (Cons x xs) (Cons y ys) = x == y && isPrefixOf' xs ys

isInfixOf' :: (Eq a) => MyList a -> MyList a -> Bool
isInfixOf' Nil _ = True
isInfixOf' (Cons _ _) Nil = False
isInfixOf' ls@(Cons x xs) ls'@(Cons y ys) =
  -- (x == y && isPrefixOf' xs ys) || isInfixOf' ls ys
  isPrefixOf' ls ls' || isInfixOf' ls ys
 

-- Module 1 Lecture 3
-- Recusion with Accumulation
length' :: MyList a -> Int
length' = undefined

sum' :: MyList Int -> Int
sum' = undefined

product' :: MyList Int -> Int
product' = undefined

maximum' :: (Ord a) => MyList a -> a
maximum' = undefined

minimum' :: (Ord a) => MyList a -> a
minimum' = undefined

elemIndex' :: (Eq a) => a -> MyList a -> Maybe Int
elemIndex' = undefined

-- Module 1 Lecture 4
-- Tail Recursion
sum'' :: MyList Int -> Int
sum'' = undefined

product'' :: MyList Int -> Int
product'' = undefined

and'' :: MyList Bool -> Bool
and'' = undefined

or'' :: MyList Bool -> Bool
or'' = undefined

any'' :: (a -> Bool) -> MyList a -> Bool
any'' = undefined

all'' :: (a -> Bool) -> MyList a -> Bool
all'' = undefined

maximum'' :: (Ord a) => MyList a -> a
maximum'' = undefined

minimum'' :: (Ord a) => MyList a -> a
minimum'' = undefined

elemIndex'' :: (Eq a) => a -> MyList a -> Maybe Int
elemIndex'' = undefined

-- Module 1 Lecture 5
-- List Accumulation
reverse' :: MyList a -> MyList a
reverse' = undefined

append' :: MyList a -> MyList a -> MyList a
append' = undefined

findIndices' :: (a -> Bool) -> MyList a -> MyList Int
findIndices' = undefined

isSuffixOf' :: (Eq a) => MyList a -> MyList a -> Bool
isSuffixOf' = undefined

map' :: (a -> b) -> MyList a -> MyList b
map' = undefined

filter' :: (a -> Bool) -> MyList a -> MyList a
filter' = undefined

snoc' :: MyList a -> a -> MyList a
snoc' = undefined

init' :: MyList a -> MyList a
init' = undefined

concat' :: MyList (MyList a) -> MyList a
concat' = undefined

-- E.g. concatMap' (\a -> [a + 1, a + 2, a + 3]) [1, 5] = [2, 3, 4, 6, 7, 9]
concatMap' :: (a -> MyList b) -> MyList a -> MyList b
concatMap' = undefined

zip' :: MyList a -> MyList b -> MyList (a, b)
zip' = undefined

-- Module 1 Lecture 6
-- Folding
foldl'' :: (b -> a -> b) -> b -> MyList a -> b
foldl'' = undefined

foldr' :: (a -> b -> b) -> b -> MyList a -> b
foldr' = undefined

scanl' :: (b -> a -> b) -> b -> MyList a -> MyList b
scanl' = undefined

sum''' :: MyList Int -> Int
sum''' = undefined

map'' :: (a -> b) -> MyList a -> MyList b
map'' = undefined

-- Module 1 Lecture 7
-- Sorting, Grouping and HOF Patterns
maximumBy' :: (a -> a -> Ordering) -> MyList a -> a
maximumBy' = undefined

sortBy' :: (a -> a -> Ordering) -> MyList a -> MyList a
sortBy' = undefined

sort' :: (Ord a) => MyList a -> MyList a
sort' = undefined

sortOn' :: (Ord b) => (a -> b) -> MyList a -> MyList a
sortOn' = undefined

groupBy' :: (a -> a -> Bool) -> MyList a -> MyList (MyList a)
groupBy' = undefined

group' :: (Eq a) => MyList a -> MyList (MyList a)
group' = undefined

-- Module 1 Lecture 8
-- Set Functions
nub' :: (Eq a) => MyList a -> MyList a
nub' = undefined

delete' :: (Eq a) => a -> MyList a -> MyList a
delete' = undefined

intersect' :: (Eq a) => MyList a -> MyList a -> MyList a
intersect' = undefined

union' :: (Eq a) => MyList a -> MyList a -> MyList a
union' = undefined

-- Module 1 Lecture 9
-- Monadic Functions
mapM' :: (Monad m) => (a -> m b) -> MyList a -> m (MyList b)
mapM' = undefined

foldM' :: (Monad m) => (b -> a -> m b) -> b -> MyList a -> m b
foldM' = undefined

sequence' :: (Monad m) => MyList (m a) -> m (MyList a)
sequence' = undefined
