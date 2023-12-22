{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module M1Lecture9 where

import Control.Monad (foldM)
import Control.Monad.Logger
import Control.Monad.State
import qualified Data.List as L
import Data.Text (pack)

import Utils

commonToken :: (MonadFail m) => [String] -> m String
commonToken = go [] 
    where 
        go accum [] = return $ reverse $ concat accum
        go accum (x:y:z:rest) = 
            case L.intersect (L.intersect x y) z of
                [i] -> go ([i]:accum) rest
                _ -> fail "Multiple common tokens"
        go _ _ = fail "Number of strings not divisible by 3"

compartmentalize :: (MonadFail m) => String -> Int -> m Char
compartmentalize input numBuckets = 
    case compartmentsOf input  of
        [] -> fail "Empty input"
        (str:strs) -> case foldr L.intersect str strs of
            [i] -> return i
            _ -> fail "Multiple characters found"
    where
        compartmentsOf str  = go [] str
            where 
                n = length str `div` numBuckets

                go accum "" = accum
                go accum s =
                    let 
                        (compartment, rest) = L.splitAt n s
                    in go (compartment : accum) rest

anyOverlap :: (MonadLogger m) => [(Int, Int)] -> m Bool
anyOverlap intervals = 
    overLap $ L.sortOn fst intervals
  where
    overLap :: (MonadLogger m) => [(Int, Int)] -> m Bool
    overLap [] = do
        logDebugN $ pack "No overlap found!"
        return False
    overLap [_] = do
        logDebugN $ pack "No overlap found!"
        return False
    overLap ((x1, x2) : rest@((y1, y2) : zs)) =
        if y1 <= x2 then do
            logDebugN $ pack $ show (x1, x2) <> " and " <> show (y1, y2) <> " overlap!"
            return True
        else do
            logDebugN $ pack $ show (x1, x2) <> " and " <> show (y1, y2) <> " do not overlap!"
            overLap rest


traverse2D :: (MonadLogger m) => Coord2 -> [Direction4] -> m Int
traverse2D start dirs = do
    length . L.nub . snd <$> foldM f (start, [start]) dirs
    where
        f (coord, coords) dir = do
            let next = stepD4 coord dir
            logDebugN $ pack $ "Visiting: " <> show next
            return (next, next : coords)

data ListQueue a = ListQueue
  { insert :: [a]
  , remove :: [a]
  } deriving Show

enqueue :: (MonadState (ListQueue a) m) => a -> m ()
enqueue x = do
    queue <- get
    put $ queue { insert = x : insert queue }
    return ()

dequeue :: (MonadState (ListQueue a) m) => m (Maybe a)
dequeue = do
    queue <- get
    case remove queue of
        [] -> case insert queue of
            [] -> return Nothing
            i -> do
                let (i' : is) = reverse i
                put $ queue { insert = [], remove = is }
                return $ Just i'
        (r : rs) -> do
            put queue { remove = rs }
            return $ Just r


data Parity = Even | Odd

math2d :: (MonadFail m, MonadLogger m, MonadState Int m) => [[Int]] -> m ()
math2d [] = return ()
math2d rows'@(row : rows) = do
    if any (/= length row) (map length rows) 
        then fail "Not all rows are the same length"
        else do 
            mapM_ math1d (zip [1..] rows')
            s <- get
            return ()
    where 
        math1d :: (MonadLogger m, MonadState Int m) => (Int, [Int]) -> m Int
        math1d (i, row) = do
            foldM_ f Even row
            result <- get
            logDebugN $ pack $ "After row " <> show i <> ", value is " <> show result
            return result

        f :: (MonadState Int m) => Parity -> Int -> m Parity
        f Even y = modify (+ y) >> return Odd
        f Odd y = modify (* y) >> return Even
