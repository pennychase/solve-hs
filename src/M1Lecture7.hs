module M1Lecture7 where

import Data.Function
import qualified Data.List as L

data Student = Student
  { name :: String
  , mathScore :: Int
  , compositionScore :: Int
  , foreignLanguageScore :: Int
  , scienceScore :: Int
  , literatureScore :: Int
  } deriving Show

studentAwards :: [Student] -> (String, String, String)
studentAwards [] = ("", "", "")
studentAwards students = ( name $ L.maximumBy (on compare mathScore) students, 
                           name $ L.maximumBy (on compare scienceScore) students,
                           name $ L.maximumBy (on compare literatureScore) students)

anyOverlap :: [(Int, Int)] -> Bool
anyOverlap intervals = overLap $ L.sortOn fst intervals
  where
    overLap [] = False
    overLap [_] = False
    overLap ((x1, x2) : rest@((y1, y2) : zs)) =
      if y1 <= x2 then True else overLap rest


buildIntervals :: Int -> [Bool] -> [(Int, Int)]
buildIntervals _ [] = []
buildIntervals startIndex values = 
  go startIndex [] (L.group values)
  where 
    go start intervals [] = reverse $ intervals 
    go start intervals (first : rest) 
      | head first = go (start + len) ((start, start + len - 1) : intervals) rest
      | otherwise = go (start + len) intervals rest
      where
        len = length first


anagrams :: [String] -> [[String]]
anagrams inputs = L.sort $ map (L.sort . map fst) groupedAnagrams  
  where
    -- zip sorted strings with the originals in order to group them into anagrams based on sorted strings
    -- We have a list of lists of tuples. Each list of tuples is a collection of anagrams, fst is the original
    -- string and snd is the sorted string (used for comparison)
    groupedAnagrams :: [[(String, String)]]
    groupedAnagrams = L.groupBy (\x y -> snd x == snd y) $ L.sortOn snd $ zip inputs (map L.sort inputs)

buildMaze :: String -> Int -> [[((Int, Int), Bool)]]
buildMaze mazeString numRows = 
  L.groupBy (on (==) (fst . fst)) $ zip coords (map cToBool mazeString)
    where
    numCols = length mazeString `div` numRows
    coords = [ (r, c) | r <- [0 .. numRows - 1], c <- [0 .. numCols - 1]]
    cToBool c = 
      case c of 
        '.' -> False
        'x' -> True


incrementingChunks :: [Int] -> [[Int]]
incrementingChunks numbers = groupBy''(\x y -> x + 1 == y) numbers

-- Impemented the groupBy' in MyList for regular lists - it tests adjacent elements
groupBy'' :: (a -> a -> Bool) ->  [a] -> [[a]]
groupBy'' _ [] = []
groupBy'' f (x : xs) = reverse $ go [x] [] xs
  where
    go grp grps [] =  reverse grp : grps
    go grp grps (x : xs) =
      if f (head grp) x
        then go (x : grp) grps xs
        else go [x] (reverse grp : grps) xs
