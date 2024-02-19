{-# LANGUAGE ScopedTypeVariables #-}

module M2Lecture4 where

import Control.Monad.Logger
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.List as L
import qualified Data.MultiMap as MM
import qualified Data.Set as S
import qualified Data.Text as T

import M1Lecture10
import Utils

hiddenMessage :: (MonadLogger m) => [String] -> m String
hiddenMessage inputs = undefined
 
mazeDrilling :: (MonadLogger m) => Coord2 -> [Coord2] ->
 (Coord2, [Direction4]) -> (Coord2, [Direction4]) -> m (Coord2, Coord2, Coord2)
mazeDrilling (width, height) obstacles (start1, dirs1) (start2, dirs2) = do
  pure (coord1, coord2, coord3) 
  where
    coord1 = moves obstacles1 start1 dirs1
    (coord2, obstacles2) = drills obstacles1 start2 dirs2
    coord3 = moves obstacles2 start1 dirs1

    obstacles1 = HS.fromList obstacles

    isWall (x,y) = x < 0 || x >= width || y < 0 || y >= height

    moves obstacles position [] = position
    moves obstacles position (d:ds) = moves obstacles (move d) ds  
      where
        move dir = 
          let
            newPos = stepD4 position dir
          in if HS.member newPos obstacles || isWall newPos
            then position else newPos

    drills obstacles position [] = (position, obstacles)
    drills obstacles position (d:ds) = 
      let
        newPos = stepD4 position d
        position' = if isWall newPos then position else newPos
        obstacles' = if HS.member position' obstacles then HS.delete position' obstacles else obstacles
      in drills obstacles' position' ds

schoolyardRace :: (MonadLogger m) => Int -> [Coord2] -> [Coord2] -> [Coord2] -> m (Int, Int, Int, Int)
schoolyardRace width team1s team2s obstacles = undefined

type Strike = (Coord2, Int, Direction8)

powerStrikes :: (MonadLogger m) => Coord2 -> [Strike] -> m Int
powerStrikes (numRows, numCols) strikes = undefined

edgePower :: (MonadLogger m) => [(String, String, Int)] -> String -> m Int
edgePower edges start = do
  pure $ getResult $ f start (cycle [product, sum]) HS.empty HS.empty 0
  where
    
    graph = mkUndirectedGraph edges

    f n fns edgesVisited nodesVisited accum =
      if HS.member n nodesVisited 
        then (accum, edgesVisited, nodesVisited)
        else
          let
            neighbors = filter (\x -> not (HS.member (n, fst x) edgesVisited))
                               (graphNeighborCosts graph n)
            result = if null neighbors then 0 else head fns $ map snd neighbors
            edgesVisited' = foldr HS.insert edgesVisited (concatMap ((\y -> [(n, y), (y, n)]) . fst) neighbors)
            nodesVisited' = HS.insert n nodesVisited
          in
            g (graphNeighbors graph n) (tail fns) edgesVisited' nodesVisited' (accum+result)

    g [] _ edgesVisited nodesVisited accum = (accum, edgesVisited, nodesVisited)
    g nodes@(n:ns) fns edgesVisited nodesVisited accum = 
        let (result, edgesVisited', nodesVisited') = 
               foldr (\x (acc, es, ns) -> f x fns es ns acc) (accum, edgesVisited, nodesVisited) nodes
        in g ns fns edgesVisited' nodesVisited' result

    getResult (r, _, _) = r
  
edgePower' :: (MonadLogger m) => [(String, String, Int)] -> String -> m Int
edgePower' edges start = do
  pure $ getResult $ f start (cycle [product, sum]) HS.empty HS.empty
  where
    
    graph = mkUndirectedGraph edges

    f n fns edgesVisited nodesVisited =
      if HS.member n nodesVisited 
        then (0, edgesVisited, nodesVisited)
        else
          let
            neighbors = filter (\x -> not (HS.member (n, fst x) edgesVisited))
                               (graphNeighborCosts graph n)
            result = if null neighbors then 0 else head fns $ map snd neighbors
            edgesVisited' = foldr HS.insert edgesVisited (concatMap ((\y -> [(n, y), (y, n)]) . fst) neighbors)
            nodesVisited' = HS.insert n nodesVisited
          in
            trace ("Node: " <> show n <> ", Result: " <> show result) (g (graphNeighbors graph n) (tail fns) edgesVisited' nodesVisited' result)

    g [] _ edgesVisited nodesVisited accum = (accum, edgesVisited, nodesVisited)
    g (n:ns) fns edgesVisited nodesVisited accum = 
        let (result, edgesVisited', nodesVisited') = f n fns edgesVisited nodesVisited
        in g ns fns edgesVisited' nodesVisited' (accum + result)

    getResult (r, _, _) = r





