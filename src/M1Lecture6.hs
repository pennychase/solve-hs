module M1Lecture6 where

import Prelude hiding (Either(..))
import Utils
import Data.Maybe (catMaybes)
import Data.List.Extra ((!?))

volumeAndSurfaceArea :: [(Int, Int, Int)] -> (Int, Int)
volumeAndSurfaceArea  =
    foldr (\d (v, sa) -> (volume d + v, surfaceArea d + sa)) (0, 0)
    where
        volume (w, l, h) = w * l * h
        surfaceArea (w, l, h) = 2 * (w * l + l * h + w * h)

countMostAdjacent :: [(Int, Int)] -> Int
countMostAdjacent [] = 0
countMostAdjacent (x@(p,c):xs) = snd $ foldl f (x,c) xs
    where
        f ((p1, c1), lastMax) (p2, c2) =
            if p1 + 1 == p2
                then ((p2, c2), max lastMax (c1 + c2))
                else ((p2, c2), max c2 lastMax)


robotHits :: Int -> Int -> [Int] -> (Int, Int, Int)
robotHits x0 y0 spikes = counts
    where
        (_, _, counts) = foldl step (x0, y0, (0, 0, 0)) spikes
        step :: (Int, Int, (Int, Int, Int)) -> Int -> (Int, Int, (Int, Int, Int))
        step (r1, r2, (w1, w2, tie)) spike = newState r1 r2
            where
                dist = min (abs (r1 - spike)) (abs (r2 - spike))
                newPos p d = if p <= spike then p + d else  p - d
                newState p1 p2
                    | p1' == spike && p2' == spike = (p1' - 2, p2' + 2, (w1, w2, tie + 1))
                    | p1' == spike = (p1', p2', (w1 + 1, w2, tie))
                    | p2' == spike = (p1', p2', (w1, w2 + 1, tie))
                    | otherwise = error "Sould not get here"
                    where
                        (p1', p2') = (newPos p1 dist, newPos p2 dist)

data Parity = Even | Odd

math2d :: [[Int]] -> Int
math2d = foldl math1d 0
    where
        math1d :: Int -> [Int] -> Int
        math1d prev row = fst $ foldl f (prev, Even) row

        f :: (Int, Parity) -> Int -> (Int, Parity)
        f (x, Even) y = (x + y, Odd)
        f (x, Odd) y = (x * y, Even)

seamCarving :: [[Int]] -> Int
seamCarving [] = 0
seamCarving (r:rs) = minimum $ foldl carveRow r rs
    where
        indices = [0 .. length r - 1]

        carveRow :: [Int] -> [Int] -> [Int]
        carveRow minEnergy row = map (\i -> row !! i + findMin i) indices
            where
                findMin i = minimum $ catMaybes [ minEnergy !? (i - 1), minEnergy !? i, minEnergy !? (i + 1)]


manhattanTravel2d :: [Coord2f] -> Double -> Coord2f
manhattanTravel2d = undefined
