module M1Lecture1 where

import Utils

-- Write solution using pattern matching (I'd actually write this as: sum3 = sum . take 3)
sum3 :: [Int] -> Int
sum3 [] = 0
sum3 [x] = x
sum3 [x,y] = x + y
sum3 (x : y : z : _) = x + y + z

firstTravel :: [Coord2] -> Int
firstTravel (x : y : _) = manhattanDistance x y
firstTravel _ = 0