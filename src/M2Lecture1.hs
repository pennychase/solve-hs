{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module M2Lecture1 where

import Control.Monad.Fail
import Control.Monad.Logger
import qualified Data.Set as S
import Data.List (sort)
import Data.Sequence (Seq( (:<|)), (|>))
import qualified Data.Sequence as Seq

import Utils

compartmentalize :: (MonadFail m) => String -> Int -> m Char
compartmentalize [] _ = fail "Empty input"
compartmentalize input numBuckets = 
  case quotRem (length input) numBuckets of
    (len, 0) -> findIntersection len input
    (_, _) -> fail "Input is not a multiple of numBuckets"
    where
      findIntersection :: (MonadFail m) => Int -> String -> m Char
      findIntersection len input
        | S.null i = fail "No common character"
        | S.size i > 1 = fail "Multiple commmon characters"
        | otherwise = pure $ head $ S.elems i
        where
          cs = chunks len input
          i = foldr S.intersection (head cs) (tail cs)
      chunks :: Int -> String -> [S.Set Char]
      chunks _ [] = []
      chunks n xs = S.fromList chunk : chunks n rest
        where
          (chunk, rest) = splitAt n xs


traverse2D4P :: Coord2 -> [Direction4] -> Int
traverse2D4P start dirs = S.size . snd $ foldr move1P (players, visitedCoords) dirs
  where
    players = Seq.fromList $ replicate 4 start
    visitedCoords = S.singleton start

    move1P :: Direction4 -> (Seq Coord2, S.Set Coord2) -> (Seq Coord2, S.Set Coord2) 
    move1P dir ( position :<| positions, coords) =
      let coord' = stepD4 position dir
      in (positions |> coord', S.insert coord' coords)

data LightCommand = On | Off | Toggle
  deriving (Show, Eq)

toggleLights :: [(LightCommand, Word, Word)] -> Int
toggleLights cmds = 
  S.size $ foldl process S.empty cmds
  where
    process :: S.Set Word -> (LightCommand, Word, Word) -> S.Set Word
    process onLights (On, start, end)  = S.fromList [start .. end] `S.union` onLights
    process onLights (Off, start, end) = onLights `S.difference` S.fromList [start .. end]
    process onLights (Toggle, start, end) = (onLights `S.union` turnOn) `S.difference` turnOff
      where
        changes = S.fromList [start .. end]
        turnOff = onLights `S.intersection` changes
        turnOn = changes `S.difference` turnOff

treeVisibility :: (MonadLogger m) =>
  (Coord2, Coord2, Coord2) -> ([Coord2], [Coord2], [Coord2]) -> m Int
treeVisibility (l1, l2, l3) (tree1s, tree2s, tree3s) = undefined

collectPellets :: (MonadLogger m) => [Coord2] -> [(Direction4, Word)] -> m Int
collectPellets pellets dirs = undefined

gameOfLife :: (MonadLogger m) => Coord2 -> S.Set Coord2 -> m (S.Set Coord2)
gameOfLife (numCols, numRows) aliveSet = undefined
