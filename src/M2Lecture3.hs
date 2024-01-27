{-# LANGUAGE OverloadedStrings #-}

module M2Lecture3 where

import Control.Monad.Logger
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Ord as O
import qualified Data.Text as T

import M1Lecture10
import Utils

pickBestClump :: [Int] -> Int
pickBestClump [] = 0
pickBestClump values = foldl pickClump 0 [minVal - 1 .. maxVal + 1]
  where
    occMap = foldl incKey emptyOcc values
    (minVal, _) = M.findMin occMap
    (maxVal, _) = M.findMax occMap

    getCount x = M.findWithDefault 0 x occMap

    pickClump m x = if score > m then score else m
      where
        score = x * (getCount (x - 1) + getCount x + getCount (x + 1))

directoryCounts :: (MonadLogger m) => [String] -> m [(String, Word)]
directoryCounts inputs = return $ L.sortBy sortTuples $ M.toList $ foldl countPath emptyOcc inputs
  where
    -- Descending sort on counts and break ties with lexicographic sort of dir names
    sortTuples (s1, c1) (s2, c2) =
      case compare c1 c2 of
        LT -> GT
        GT -> LT
        EQ -> compare s1 s2
    -- split on "/" to create list of path elements. Reverse the list and drop the file, then recursively
    -- count the items by adding 1 to the current item (current is the items beneath the current directory)
    -- unless we've already seen it in another path (so we don't double count).
    countPath :: OccMap String -> String -> OccMap String
    countPath occMap path = go 1 occMap dirs
      where
        dirs = tail . map T.unpack . reverse $ T.splitOn "/" (T.tail (T.pack path)) -- strip leading "/" before split
        go numItems counts [] = counts
        go numItems counts (d:ds) = 
          let
            numItems' = if M.member d counts then numItems else numItems + 1
          in go numItems' (addKey counts d numItems) ds

algaeGrowth :: Int -> Int -> [Int] -> Integer
algaeGrowth days growthRate startingValues = sum . M.elems $ evolveState update initial days
  where
    initial :: OccMapBig Int
    initial = foldl incKey emptyOcc startingValues

    update prevMap = 
      case M.lookup 0 newMap of
        Nothing -> newMap
        Just count -> M.delete 0 $ addKey newMap growthRate (2 * count)
      where
        newMap = M.mapKeys pred prevMap

data SparseMatrix = SparseMatrix
  { nRows :: Word
  , nCols :: Word
  , values :: M.Map (Word, Word) Double
  } deriving (Show, Eq)
  
empty :: Word -> Word -> SparseMatrix
empty numRows numCols = SparseMatrix numRows numCols M.empty

set :: Word -> Word -> Double -> SparseMatrix -> SparseMatrix
set row col val sparseMatrix = 
  if outOfBounds row col sparseMatrix
    then error $ "Index is out of bounds: " <> show (row, col)
    else sparseMatrix { values = values' }
  where
    values' = if val == 0.0
                then M.delete (row, col) (values sparseMatrix)
                else M.insert (row, col) val (values sparseMatrix)

outOfBounds :: Word -> Word -> SparseMatrix -> Bool
outOfBounds row col sparseMatrix =
  row < 0 || row >= nRows sparseMatrix || col < 0 || col >= nCols sparseMatrix

get :: Word -> Word -> SparseMatrix -> Double
get row col sparseMatrix = 
  if outOfBounds row col sparseMatrix
    then error $ "Index is out of bounds: " <> show (row, col)
    else case M.lookup (row, col) (values sparseMatrix) of
      Nothing -> 0.0
      Just val -> val

get' :: Word -> Word -> SparseMatrix -> Maybe Double
get' row col sparseMatrix = 
  if outOfBounds row col sparseMatrix
    then Nothing
    else case M.lookup (row, col) (values sparseMatrix) of
      Nothing -> Just 0.0
      Just val -> Just val

add :: SparseMatrix -> SparseMatrix -> SparseMatrix
add matrix1 matrix2 =
  if (nRows matrix1 /= nRows matrix2 || nCols matrix1 /= nCols matrix2)
    then error "Matrices do not have the same dimensions"
    else matrix1 { values = M.unionWith (+) (values matrix1) (values matrix2)}
  
add' :: SparseMatrix -> SparseMatrix -> Maybe SparseMatrix
add' matrix1 matrix2 = 
    if (nRows matrix1 /= nRows matrix2 || nCols matrix1 /= nCols matrix2)
      then Nothing
      else Just $ add matrix1 matrix2

-- For each row, compute the dot products with each column in the second matrix that has data
-- Each column is constructed by transposing the row and using the column number, i.e., the column's rows  
-- are the columns of the row and the column's columns is the column number
multiply :: SparseMatrix -> SparseMatrix -> SparseMatrix
multiply matrix1 matrix2 = 
  if nRows matrix1 /= nCols matrix2
    then error "Number of rows of first matrix not equal to number of columns of second"
    else foldr (\r ms -> multRow r colNums ms) (empty (nRows matrix1) (nCols matrix2)) rows

  where

    rows = L.groupBy (\x y -> fst x == fst y) $ M.keys (values matrix1)
    colNums = L.nub $ map snd (M.keys (values matrix2))

    makeColumn row colNum = zip (map snd row) (repeat colNum)

    dotProd row col = sum $ zipWith (\(i1,j1) (i2, j2) -> (get i1 j1 matrix1) * (get i2 j2 matrix2)) row col

    multRow :: [(Word, Word)] -> [Word] -> SparseMatrix -> SparseMatrix
    multRow row cns matrix = foldr f matrix cns
      where
        f cn m =
          let
            rn = (fst . head) row
            val = dotProd row (makeColumn row cn)
          in set rn cn val m


multiply' :: SparseMatrix -> SparseMatrix -> Maybe SparseMatrix
multiply' matrix1 matrix2 = 
    if nRows matrix1 /= nCols matrix2
    then Nothing
    else Just $ multiply matrix1 matrix2

data Op = Add | Sub | Mult | Div
  deriving (Show, Eq)

data Instruction =
  Dep String String Op |
  Assign Int

calculationGraph :: String -> [(String, Instruction)] -> Int
calculationGraph root instrs = calculate root
  where
    graph = M.fromList instrs
    calculate node = 
      case M.lookup node graph of
        Nothing -> error "Unknown variable"
        Just (Assign val) -> val
        Just (Dep arg1 arg2 op) -> apply op (calculate arg1) (calculate arg2)
    apply op arg1 arg2 =
      case op of
        Add -> arg1 + arg2
        Sub -> arg1 - arg2
        Mult -> arg1 * arg2
        Div -> arg1 `div` arg2
