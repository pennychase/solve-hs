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
and' (Cons x xs) = x && and' xs

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
  isPrefixOf' ls ls' || isInfixOf' ls ys


-- Module 1 Lecture 3
-- Recusion with Accumulation
length' :: MyList a -> Int
length' Nil = 0
length' (Cons x xs) = 1 + length' xs

sum' :: MyList Int -> Int
sum' Nil = 0
sum' (Cons x xs) = x + sum' xs

product' :: MyList Int -> Int
product' Nil = 1
product' (Cons x xs) = x * product' xs

maximum' :: (Ord a) => MyList a -> a
maximum' Nil = error "Can't apply maximum' to empty list"
maximum' (Cons x Nil) = x
maximum' (Cons x xs) = max x (maximum' xs)

minimum' :: (Ord a) => MyList a -> a
minimum' Nil = error "Can't apply minimum' to empty list"
minimum' (Cons x Nil) = x
minimum' (Cons x xs) = min x (minimum' xs)

elemIndex' :: (Eq a) => a -> MyList a -> Maybe Int
elemIndex' _ Nil = Nothing
elemIndex' y (Cons x xs) =
  if x == y
    then Just 0
    else (1+) <$> elemIndex' y xs

-- Module 1 Lecture 4
-- Tail Recursion
sum'' :: MyList Int -> Int
sum'' = go 0
  where
    go accum Nil = accum
    go accum (Cons x xs) = go (x + accum) xs

product'' :: MyList Int -> Int
product'' = go 1
  where
    go accum Nil = accum
    go accum (Cons x xs) = go (x * accum) xs

and'' :: MyList Bool -> Bool
and'' = go True
  where
    go accum Nil = accum
    go accum (Cons x xs) = go (x && accum) xs

or'' :: MyList Bool -> Bool
or'' = go False
  where
    go accum Nil = accum
    go accum (Cons x xs) = go (x || accum) xs

any'' :: (a -> Bool) -> MyList a -> Bool
any'' pred = go False
  where
    go accum Nil = accum
    go accum (Cons x xs) = go (pred x || accum) xs

all'' :: (a -> Bool) -> MyList a -> Bool
all'' pred = go True
  where
    go accum Nil = accum
    go accum (Cons x xs) = go (pred x && accum) xs

maximum'' :: (Ord a) => MyList a -> a
maximum'' Nil = error "No elements"
maximum'' (Cons x xs) = go x xs
  where
    go accum Nil = accum
    go accum (Cons x xs) = go (max x accum) xs

minimum'' :: (Ord a) => MyList a -> a
minimum'' Nil = error "No elements"
minimum'' (Cons x xs) = go x xs
  where
    go accum Nil = accum
    go accum (Cons x xs) = go (min x accum) xs

elemIndex'' :: (Eq a) => a -> MyList a -> Maybe Int
elemIndex'' e = go (Just 0)
  where
    go accum Nil = Nothing
    go accum (Cons x xs) =
      if x == e
        then accum
        else go ((+1) <$> accum) xs

-- Module 1 Lecture 5
-- List Accumulation
reverse' :: MyList a -> MyList a
reverse' = go Nil
  where
    go accum Nil = accum
    go accum (Cons x xs) = go (Cons x accum) xs

append' :: MyList a -> MyList a -> MyList a
append' lis1 lis2 = go lis2 (reverse' lis1) 
  where
    go accum Nil = accum
    go accum (Cons x xs) = go (Cons x accum) xs

findIndices' :: (a -> Bool) -> MyList a -> MyList Int
findIndices' pred = go 0 Nil 
  where
    go _ accum Nil = reverse' accum
    go i accum (Cons x xs) =
      if pred x
        then go (i + 1) (Cons i accum) xs
        else go (i + 1) accum xs 

isSuffixOf' :: (Eq a) => MyList a -> MyList a -> Bool
isSuffixOf' lis1 lis2 = isPrefixOf' (reverse' lis1) (reverse' lis2)

map' :: (a -> b) -> MyList a -> MyList b
map' f = go Nil 
  where
    go accum Nil = reverse' accum
    go accum (Cons x xs) = go (Cons (f x) accum) xs

filter' :: (a -> Bool) -> MyList a -> MyList a
filter' pred = go Nil
  where
    go accum Nil = reverse' accum
    go accum (Cons x xs) =
      if pred x
        then go (Cons x accum) xs
        else go accum xs

snoc' :: MyList a -> a -> MyList a
snoc' lis = go Nil lis
  where
    go accum Nil a = reverse' (Cons a accum)
    go accum (Cons x xs) a = go (Cons x accum) xs a


init' :: MyList a -> MyList a
init' lis = go Nil lis
  where 
    go accum lis =
      case lis of
        Nil -> error "Can't take init of empty list"
        (Cons x Nil) -> reverse' accum
        (Cons x xs) -> go (Cons x accum) xs

concat' :: MyList (MyList a) -> MyList a
concat' = go Nil
  where
    go accum Nil = reverse' accum
    go accum (Cons l ls) = go (append' (reverse' l) accum) ls

-- E.g. concatMap' (\a -> [a + 1, a + 2, a + 3]) [1, 5] = [2, 3, 4, 6, 7, 9]
concatMap' :: (a -> MyList b) -> MyList a -> MyList b
concatMap' f ls = concat' (map' f ls)

zip' :: MyList a -> MyList b -> MyList (a, b)
zip' = go Nil
  where
    go accum Nil _ = reverse' accum
    go accum _ Nil = reverse' accum
    go accum (Cons x xs) (Cons y ys) = go (Cons (x, y) accum) xs ys

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
