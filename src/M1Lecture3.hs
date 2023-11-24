module M1Lecture3 where

import Data.List (isPrefixOf)

makeDigits :: Word -> [Word]
makeDigits 0 = []
makeDigits num = 
    num `mod` 10 : makeDigits (num `div` 10)

buildNumber :: [Word] -> Word
buildNumber [] = 0
buildNumber (d:ds) = d * (10 ^ length ds) + buildNumber ds

elevation :: String -> Int
elevation [] = 0
elevation (cmd : cmds) =
    case cmd of
        'u' -> 100 + elevation cmds
        'd' -> elevation cmds - 100
        _   -> error "Unknown command"

viralCount :: Int -> Int -> Int
viralCount _ 0 = 0
viralCount friends hours = 
    let
        friends' = friends `div` 3
    in friends' + viralCount (4 * friends') (hours - 1)

addQuads :: [Int] -> [Int] -> Int
addQuads [] [] = 0
addQuads xs ys = 
    let
        (nums1, rest1) = splitAt 2 xs
        (nums2, rest2) = splitAt 2 ys
    in product (nums1 <> nums2) + addQuads rest1 rest2

countChars :: String -> Int
countChars "" = 0
countChars ('\\' : 'x' : _ : _ : cs) = 1 + countChars cs
countChars ('\\' : 'o' : _ : _ : _ : cs) = 1 + countChars cs
countChars ('\\' : _ : cs ) = 1 + countChars cs
countChars (_ : cs) = 1 + countChars cs
