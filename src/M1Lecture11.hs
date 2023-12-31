{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module M1Lecture11 where

import Control.Monad (foldM)
import Control.Monad.Logger
import Data.List (sortOn, sort, nub, elemIndices, tails, isPrefixOf, transpose, findIndex, findIndices, delete)
import Data.Text (pack)

-- To run: runStdoutLoggingT $ countIntervalsExcludingSpikes [(1,3), (5,8), (6,10)] [1, 2, 3]
countIntervalsExcludingSpikes :: (MonadLogger m) => [(Int, Int)] -> [Int] -> m Int
countIntervalsExcludingSpikes intervals spikes = do
  logDebugN $ pack $ "running"
  return $ intervalSizes intervals' - length (filter (\s -> any (inInterval s) intervals') spikes)
    where
      intervals' = makeDisjointIntervals intervals

      inInterval :: Int -> (Int, Int) -> Bool
      inInterval n (s, e) = n >= s && n <= e

      intervalSizes :: [(Int, Int)]  -> Int
      intervalSizes = foldr (\(x, y) zs -> (y-x+1) + zs) 0

      makeDisjointIntervals :: [(Int, Int)] -> [(Int, Int)]
      makeDisjointIntervals intervals = reverse $ go [] (sortOn fst intervals)
        where
          go accum [] = accum
          go accum [i] = i : accum
          go accum (i1 : i2 : rest) =
            let
              xs = makeDisjoint i1 i2
            in go (head xs : accum)  ((tail xs) <> rest)
          makeDisjoint (s1, e1) (s2, e2)
            | e1 <= s2 = [(s1, e1), (s2, e2)]
            | e1 > s2 && e1 < e2 = [(s1, s2), (s2+1, e2)]
                  | e1 > s2 && e1 >= e2 = [(s1, e1)]

backpackMeals :: (MonadLogger m) => Int -> [Int] -> [Int] -> m Int
backpackMeals _ [] _ = pure 0
backpackMeals _ _ [] = pure 0
backpackMeals maxWeight breakfasts lunches =
  pure $ if res == [] then 0 else maximum res
  where
    res = filter (<= maxWeight) $ [b + l | b <- breakfasts, l <- lunches]



stackBoxes :: (MonadLogger m) => [String] -> [(Int, Int, Int)] -> m String
stackBoxes stacks [] = pure $ concat stacks
stackBoxes stacks ((s,d,n) : moves) =
  stackBoxes (insertAt d dest (insertAt s src stacks)) moves
  where
      (src, dest) = move (stacks !! s) (stacks !! d) n

      move :: String -> String -> Int -> (String, String)
      move src dest n =(newSrc, newDest)
        where
          (boxes, newSrc) = splitAt n src
          newDest = foldl (flip (<>)) dest $ chunks 2 boxes

      chunks :: Int -> [a] -> [[a]]
      chunks _ [] = []
      chunks n xs = chunk : chunks n rest
        where
          (chunk, rest) = splitAt n xs

      insertAt :: Int -> a -> [a] -> [a]
      insertAt n new lst =
        let
          (first, rest) = splitAt n lst
        in first <> (new : tail rest)


data Register = Eax | Ebx | Ecx
  deriving (Show, Eq, Enum)

data Command =
  LoadValue Register Int |
  -- First register is destination
  -- e.g. AddRegister Eax Ebx ==> Eax += Ebx
  AddRegister Register Register |
  SubRegister Register Register |
  MultRegister Register Register |
  PrintRegister Register
  deriving Show

data Machine =
  Machine {
      eax :: Int
    , ebx :: Int
    , ecx :: Int
    , output :: [Int]
  }
  deriving Show

runAssembly :: (MonadLogger m) => [Command] -> m [Int]
runAssembly commands = do
  let initial = Machine 0 0 0 []
  final <- foldM runInstr initial commands
  pure $ reverse $ output final

runInstr :: (MonadLogger m) => Machine -> Command -> m Machine
runInstr m@(Machine {..}) instr = do
  logDebugN $ pack $ show instr <> ": " <> show m
  pure $ case instr of
    PrintRegister reg -> m { output = getReg m reg : output}
    LoadValue reg val -> putReg m reg val
    AddRegister r1 r2 -> putReg m r1 (getReg m r1 + getReg m r2)
    SubRegister r1 r2 -> putReg m r1 (getReg m r1 - getReg m r2)
    MultRegister r1 r2 -> putReg m r1 (getReg m r1 * getReg m r2)


getReg :: Machine -> Register -> Int
getReg Machine {..} Eax =  eax
getReg Machine {..} Ebx =  ebx
getReg Machine {..} Ecx =  ecx

putReg :: Machine -> Register -> Int -> Machine
putReg m@Machine {..} Eax val =  m { eax = val}
putReg m@Machine {..} Ebx val =  m { ebx = val}
putReg m@Machine {..} Ecx val =  m { ecx = val}


wordSearch :: (MonadLogger m) => String -> [[Char]] -> m Bool
wordSearch input grid
  | any (searchOne input) grid = pure True                          -- rows
  | any (searchOne (reverse input)) grid = pure True                -- reverse rows
  | any (searchOne input) (transpose grid) = pure True              -- columns
  | any (searchOne (reverse input)) (transpose grid) = pure True    -- reverse columns
  | any (searchOne input) diagonals = pure True                     -- diagonals
  | any (searchOne (reverse input)) diagonals = pure True           -- reverse diagonals
  | otherwise = pure False
  where
    diagonals = diags grid <> diags (transpose (reverse grid))      -- (transpose . reverse) rotates grid

    searchOne :: String -> String -> Bool
    searchOne input str = any (uncurry isPrefixOf . (input,)) (tails str)

    diags :: [String] -> [String]
    diags grid = 
      map (map (\(x, y) -> (grid !! x) !!y)) (upperDiag <> lowerDiag)
      where
        lastRow = length grid - 1
        lastCol = length (head grid) - 1
        upperDiag = [[ (x, y) | x <- [0 .. lastRow], y <- [0 .. lastCol], y == x + i] | i <- [0 .. lastRow]]
        lowerDiag = [[(x,y) | x <- [lastRow,lastRow-1 .. 0], y <- [lastRow,lastRow-1 ..0], y == x - i] | i <- [0 .. lastRow]]

hiddenMessage :: (MonadLogger m) => [String] -> m String
hiddenMessage inputs = pure $ f "" inputs
  where 
    f accum strs =
      case findNext strs of
        Nothing -> reverse accum
        Just c -> f (c : accum) (map (delete c) strs)

    -- Finds the next letter in the message. For the first letter in each string, see if it's in the
    -- remaining letters of each string. The letter that doesn't appear in any string is the next letter
    -- (since the letters in each string appear in the correct order)
    findNext :: [String] -> Maybe Char
    findNext strs = 
      (cs !!) <$> findIndex null is
      where
        strs' = filter (not . null) strs
        cs = nub . map head $ strs'
        is = map (\x -> findIndices (elem x) (map tail strs')) cs
