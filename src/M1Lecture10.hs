module M1Lecture10 where

import Data.Function (on)
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.List as L

import Utils

evolveState :: (a -> a) -> a -> Int -> a
evolveState _ state 0 = state
evolveState f state n = evolveState f (f state) (n - 1)

evolveUntil :: (a -> a) -> a -> (a -> Bool) -> a
evolveUntil f state testDone = 
  if testDone state
    then state
    else evolveUntil f (f state) testDone

-- For testing evolveState and evolveUntil

rate :: Double
rate = 0.05

monthlyPayment :: Double
monthlyPayment = 1000.0

payMonth1 :: Double -> Double
payMonth1 principal = principal - (monthlyPayment - interest)
  where
    interest = principal * (rate / 12.0)

payMonth2 :: (Int, Double) -> (Int, Double)
payMonth2 (x, principal) = (x + 1, principal - (monthlyPayment - interest))
  where
    interest = principal * (rate / 12.0)

runFSM ::
  (Enum a) =>
  (Char -> a -> a) ->
  (a -> Bool) ->
  a ->
  String ->
  Bool
runFSM _ completionPredicate state "" = completionPredicate state
runFSM trans completionPredicate state (x : xs) =
  runFSM trans completionPredicate (trans x state) xs

-- To test FSM
data ABState = Initial | Failure | ReceivedA | ReceivedB
  deriving (Eq, Enum)

transition :: Char -> ABState -> ABState
transition 'a' Initial = ReceivedA
transition 'b' ReceivedA = ReceivedB
transition _ _ = Failure

parseAB :: String -> Bool
parseAB = runFSM transition (== ReceivedB) Initial

runPDA ::
  (Enum a) =>

  (Char -> (a, [Char]) -> (a, [Char])) ->
  (a -> Bool) ->
  a ->
  String ->
  Bool
runPDA trans predicate initial input = f (initial, []) input
  where
    f (state, _) "" = predicate state
    f cur (x:xs) = f (trans x cur) xs


fishPopulation :: Int -> Int -> Int
fishPopulation numYears fishEaten =
  evolveState f 1000 numYears
  where
    f numFish = fromIntegral numFish + floor (fromIntegral numFish * 0.1) - fishEaten

treeGrowth :: Int -> Int -> Int
treeGrowth height numYears = 
  evolveState f height numYears
  where
    f h = 2*h + 3

-- Count Cars

data Direction = NorthSouth | EastWest
  deriving (Show, Eq, Enum)

data DirState = DirState
  { carCount1 :: Int
  , newCars1 :: [Int]
  , carCount2 :: Int
  , newCars2 :: [Int]
  }

mkDirState :: [Int] -> [Int] -> DirState
mkDirState [] [] = DirState 0 [] 0 []
mkDirState (c : cs) [] = DirState c cs 0 []
mkDirState [] (c' : cs') = DirState 0 [] c' cs'
mkDirState (c : cs) (c' : cs') = DirState c cs c' cs'


data CarsState = CarsState 
  { timer :: Int
  , totalCars :: Int
  , cycleTime :: Int
  , activeDirection :: Direction
  , greenNS :: Int
  , greenEW :: Int
  , dirStates :: [DirState]
  }

countCars :: (Int, Int) -> ([Int], [Int], [Int], [Int]) -> Int -> Int
countCars timers (north, south, east, west) time = 
  totalCars $ evolveState trafficTransition initialState time
  where
    initialState = 
      CarsState
        { timer = 0
        , totalCars = 0
        , cycleTime = 0
        , activeDirection = NorthSouth
        , greenNS = fst timers
        , greenEW = snd timers
        , dirStates = [mkDirState north south, mkDirState east west]
        }

    trafficTransition :: CarsState -> CarsState
    trafficTransition state
      | cycleTime state == endCycle (activeDirection state) = 
            state { activeDirection = flipDirection (activeDirection state), cycleTime = 1, timer = timer state + 1 }
      | cycleTime state < 1 = state { cycleTime = cycleTime state + 1, timer = timer state + 1 }
      | otherwise =
          let 
            ds' = if (timer state `mod` 15) == 0
              then updateMoreCars $ dirStates state !! fromEnum (activeDirection state)
              else dirStates state !! fromEnum (activeDirection state)
            (ds'', passedCars) = updateState ds'
            newDs = case activeDirection state of
              NorthSouth -> [ds'', dirStates state !! 1]
              EastWest -> [dirStates state !! 0, ds'']
          in state { cycleTime = cycleTime state + 1, timer = timer state + 1, dirStates = newDs, 
                     totalCars = totalCars state + passedCars }
      where
        endCycle NorthSouth = greenNS state
        endCycle EastWest = greenEW state
        flipDirection NorthSouth = EastWest
        flipDirection EastWest = NorthSouth
        updateState s = (s { carCount1 = carCount1', carCount2 = carCount2' }, passed1 + passed2)
          where
            (carCount1', passed1) = if carCount1 s == 0 then (0, 0) else (carCount1 s - 1, 1)
            (carCount2', passed2) = if carCount2 s == 0 then (0, 0) else (carCount2 s - 1, 1)
        updateMoreCars :: DirState -> DirState
        updateMoreCars s = 
          let 
            (carCount1', newCars1', carCount2', newCars2') =
              case (newCars1 s, newCars2 s) of
                ([], []) -> (carCount1 s, [], carCount2 s, [])
                (c : cs, []) -> (carCount1 s + c, cs, carCount2 s, [])
                ([], c : cs) -> (carCount1 s, [], carCount2 s + c, cs)
                (c : cs, c' :cs') -> (carCount1 s + c, cs, carCount2 s + c', cs')
          in DirState carCount1' newCars1' carCount2' newCars2'
          

playPacman :: (Coord2, Coord2, Coord2) -> Bool
playPacman (blinky, inky, pinky) = undefined

-- Solve REGEX

data RegexState =
  RegexInitial | RegexFailure | RegexA | RegexB | RegexC | RegexD | RegexEFG
  deriving (Eq, Enum)

solveRegex :: String -> Bool
solveRegex = 
  runFSM regexTransition (\s -> s == RegexD || s == RegexEFG) RegexInitial
  where
    regexTransition :: Char -> RegexState -> RegexState
    regexTransition 'a' RegexInitial = RegexA
    regexTransition 'b' RegexA = RegexB
    regexTransition 'c' RegexB = RegexC
    regexTransition 'a' RegexC = RegexA
    regexTransition 'd' RegexC = RegexD
    regexTransition 'e' RegexD = RegexEFG
    regexTransition 'f' RegexD = RegexEFG
    regexTransition 'g' RegexD = RegexEFG
    regexTransition 'e' RegexEFG = RegexEFG
    regexTransition 'f' RegexEFG = RegexEFG
    regexTransition 'g' RegexEFG = RegexEFG
    regexTransition _ _ = RegexFailure

-- Solve CFG

data CFGState =
  CFGInitial |
  CFGPushA   |
  CFGPushB   |
  CFGPopB    |
  CFGPopA    |
  CFGSuccess |
  CFGFailure 
  deriving (Show, Eq, Enum)

solveCFG :: String -> Bool
solveCFG = 
    runPDA cfgTransition (\s -> s == CFGSuccess || s == CFGInitial) CFGInitial 
    where
      cfgTransition :: Char -> (CFGState, [Char]) -> (CFGState, [Char]) 
      cfgTransition c (state, stack) = case (state, stack, c) of
        (CFGInitial, [], 'a') -> (CFGPushA, ['a'])
        (CFGInitial, [], 'b') -> (CFGPushB, ['b'])
        (CFGInitial, _, _) -> (CFGFailure, [])
        (CFGPushA, stack', 'a') -> (CFGPushA, 'a' : stack')
        (CFGPushA, stack', 'b') -> (CFGPushB, 'b' : stack')
        (CFGPushA, [], 'd') -> (CFGFailure, [])
        (CFGPushA, ['a'], 'd') -> (CFGSuccess, [])
        (CFGPushA, 'a' : rest, 'd') -> (CFGPopA, rest)
        (CFGPushB, stack', 'b') -> (CFGPushB, 'b' : stack')
        (CFGPushB, [], 'c') -> (CFGFailure, [])
        (CFGPushB, ['b'], 'c') -> (CFGSuccess, [])
        (CFGPushB, 'b' : rest, 'c') -> (CFGPopB, rest)
        (CFGPopB, [], _) -> (CFGFailure, [])
        (CFGPopB, ['b'], 'c') -> (CFGSuccess, [])
        (CFGPopB, 'b' : rest, 'c') -> (CFGPopB, rest)
        (CFGPopB, ['a'], 'd') -> (CFGSuccess, [])
        (CFGPopB, 'a' : rest, 'd') -> (CFGPopA, rest)
        (CFGPopA, [], _) -> (CFGFailure, [])
        (CFGPopA, ['a'], 'd') -> (CFGSuccess, [])
        (CFGPopA, 'a' : rest, 'd') -> (CFGPopB, rest)
        (_, _, _) -> (CFGFailure, stack)
       


