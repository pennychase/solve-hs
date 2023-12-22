module M1Lecture8 where

import Data.Function
import qualified Data.List as L
import Prelude hiding (Either(..))

import Utils

commonToken :: [String] -> String
commonToken  = go []
    where 
        go :: [String] -> [String] -> String
        go accum [] = reverse $ concat accum
        go accum (x:y:z:rest) = 
            case L.intersect (L.intersect x y) z of
                [i] -> go ([i]:accum) rest
                _ -> error "Multiple common tokens"
        go _ _ = error "Number of strings not divisible by 3"
                

compartmentalize :: String -> Int -> Maybe Char
compartmentalize input numBuckets = 
    case compartmentsOf input  of
        [] -> Nothing
        (str:strs) -> case foldr L.intersect str strs of
            [i] -> Just i
            _ -> Nothing
    where
        compartmentsOf str  = go [] str
            where 
                n = length str `div` numBuckets

                go accum "" = accum
                go accum s =
                    let 
                        (compartment, rest) = L.splitAt n s
                    in go (compartment : accum) rest


traverse2D :: Coord2 -> [Direction4] -> Int
traverse2D start dirs = 
    length . L.nub . snd $ L.foldl' f (start, [start]) dirs
    where
        f (coord, coords) dir = 
            let next = stepD4 coord dir
            in (next, next : coords)
 
            
smallestMultiples :: [Int] -> [Int] -> [Int]
smallestMultiples search modulos = f [] (L.sort search) modulos
    where
        f accum _ [] = reverse accum
        f accum ns (m : ms) =
            case L.span (\x -> x `mod` m /= 0) ns of
                (first, []) -> f accum first ms
                (first, r : rs) -> f (r : accum) (first <> rs) ms

grabbingLetters :: [String] -> (Int, Int, Int) -> String
grabbingLetters bag (i1, i2, i3) = L.nub . L.sort $ grab bag [i1 - 1, i2 - 1, i3 - 1]
    where
        grab _ [] = []
        grab bag (i:is) = letters <> grab (L.delete letters bag) is
            where
                letters = bag !! i


grabbingMoreLetters :: [String] -> String
grabbingMoreLetters bag = 
    L.maximumBy cmp  $ 
        -- Compute combinations of length 3, concat into strings and remove dups, and sort the list
        L.sort . map (L.nub . L.sort . concat) $ filter ((3==) . length) (L.subsequences bag)
    where
        cmp x y = case compare (length x) (length y) of
            EQ -> case compare x y of   -- same length, so compare lexigraphically (earlier will be GT)
                LT -> GT                
                _ -> LT
            res -> res

