{-# LANGUAGE ScopedTypeVariables #-}

module M2Lecture5 where

import Control.Monad.Logger
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Ix as I
import qualified Data.HashSet as HS
import qualified Data.Set as S
import qualified Data.Vector as V

import Utils

binarySearch :: (I.Ix i, Ord a) =>
  (i -> container a -> a) -> -- Indexing Function
  (i, i) ->                  -- Range
  container a ->             -- Container
  a ->                       -- Desired Element
  Maybe i                    -- Result Index
binarySearch indexF rng@(start,end) vals comp = 
  f 0 sz
  where
    indices = V.fromList (I.range rng)
    sz = I.rangeSize rng

    f start' end' =
      if start' >= end' 
        then Nothing
        else
          case compare comp (indexF (indices V.! mid) vals) of
            EQ -> Just (indices V.! mid)
            LT -> f start' mid
            GT -> f (mid + 1) end'
      where
        mid = (end' - start') `div` 2 + start'

binarySearchV :: (Ord a) => V.Vector a -> a -> Maybe Int
binarySearchV vals  = binarySearch (flip (V.!)) (0, V.length vals - 1)  vals

binarySearchA :: (Ord a, I.Ix i) => A.Array i a -> a -> Maybe i
binarySearchA vals = binarySearch (flip (A.!)) (A.bounds vals) vals

addArrays :: (MonadLogger m) => [A.Array Int Int] -> m (A.Array Int Int)
addArrays arrays = 
  pure $ A.listArray (minIndex, maxIndex) (map addAtIndex [minIndex .. maxIndex])
  where
    minIndex = minimum (map (fst . A.bounds) arrays)
    maxIndex = maximum (map (snd . A.bounds) arrays)

    getValue :: A.Array Int Int -> Int -> Int
    getValue a i = if A.inRange (A.bounds a) i then a A.! i else 0

    addAtIndex i = foldr (\a as -> (getValue a i) + as) 0 arrays

treeVisibilityRedux :: (MonadLogger m) => (Coord2, Coord2, Coord2) -> A.Array Coord2 Int -> m Int
treeVisibilityRedux (l1, l2, l3) trees = undefined

functionPermutation :: V.Vector Int -> V.Vector Int
functionPermutation v = V.fromList $ map f [0 .. V.length v - 1]
  where
    -- v' is the inverse of the function represented by v, i.e., if v[i] = j then v'[j] = i
    v' = V.fromList $ map fst $ L.sortOn snd $ V.toList (V.indexed v)
    f i = v' V.! (v' V.! i)
      

boggleSearch :: (MonadLogger m) => A.Array (Int, Int) Char -> String -> m Bool
boggleSearch grid word = undefined

bingoBoard :: (MonadLogger m) => [Int] -> [Int] -> m Bool
bingoBoard board numbers = undefined
