module M1Lecture5 where

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

calculateVelocities :: [(Double, Double)] -> [Double]
calculateVelocities xs = zipWith f xs (tail xs)
    where
        f (p1, t1) (p2, t2) = (p2 - p1) / (t2 - t1)

palindrome :: String -> Bool
palindrome input = 
    let
        noSpacesInput = filter (/= ' ') input
    in noSpacesInput == reverse noSpacesInput


allFactors :: [Int] -> [Int]
allFactors [] = []
allFactors nums =
    filter isAFactor [1 .. maximum (nums)]
    where 
        isAFactor n = any (\x -> x `mod` n == 0) nums

makeDigits :: Word -> [Word]
makeDigits = go []
    where
        go accum 0 = accum
        go accum n = go (n `mod` 10 : accum) (n `div` 10)

canStartShed :: Int -> Int -> [Int] -> Bool
canStartShed startTime requiredPackages arrivalTimes = 
    length (filter (\x -> x <= startTime) arrivalTimes) >= requiredPackages
