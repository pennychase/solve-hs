module M1Lecture4 where

buildNumberBetter :: [Word] -> Word
buildNumberBetter = go 0
    where
        go accum [] = accum
        go accum (d:ds) = go (accum * 10 + d) ds


maxesAndMins :: [Int] -> (Int, Int)
maxesAndMins [] = (0, 0)
maxesAndMins (x:xs) = go (x, x) (0, 0) xs
    where
        go _ (numMax, numMin) [] = (numMax, numMin)
        go (curMax, curMin) (numMax, numMin) (n:ns)
            | n > curMax = go (n, curMin) (numMax + 1, numMin) ns
            | n < curMin = go (curMax, n) (numMax, numMin + 1) ns
            | otherwise = go (curMax, curMin) (numMax, numMin) ns

elevationRedux :: String -> Maybe Int
elevationRedux = go 0 0
    where
        go _ _ [] = Nothing
        go elevation index (x:xs) =
            if elevation < 0
                then Just index
                else case x of
                    'u' -> go (elevation + 100) (index + 1) xs
                    'd' -> go (elevation - 100) (index + 1) xs
                    _ -> go elevation (index +1) xs



fireworks :: (Int, Int) -> Int -> Int -> [Int] -> [Int] -> Int
fireworks (left, right) n1 n2 fws1 fws2 = (f 0 n1 fws1) + (f 0 n2 fws2)
    where
        between x l r = x >= l && x <= r

        f hits neighbor fws
            | null fws = hits
            | between (neighbor + head fws) left right = f (hits + 1) neighbor (tail fws)
            | otherwise = f hits neighbor (tail fws)

data Parity = Even | Odd

math2d :: [[Int]] -> Int
math2d = go 0 
    where
        go accum [] = accum
        go accum (x:xs) = go (rowMath accum Even x) xs

        rowMath accum parity [] = accum
        rowMath accum parity (x:xs) =
            case parity of
                Even -> rowMath (accum + x) Odd xs
                Odd -> rowMath (accum * x) Even xs

tripletSums :: [Int] -> Int
tripletSums = go 0
    where
        go accum [] = accum
        go accum [x] = accum + x
        go accum [x, y] = accum + x + y
        go accum (x:y:z:rest) = go (accum + ((x + y) `mod` z)) rest
    
                

