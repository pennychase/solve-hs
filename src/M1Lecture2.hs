module M1Lecture2 where

import Crypto.Hash.MD5 (start, update, finalize)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.List as L
import Data.Word (Word8, Word)

parallelCars :: (Word, Word) -> (Word, Word) -> Bool
parallelCars (x1, v1) (x2, v2) 
  | x1 == x2 = True
  | x1 > x2 && v1 > v2 = False
  | x2 > x1 && v2 > v1 = False
  | otherwise = parallelCars (x1 + v1, v1) (x2 + v2, v2)


findKey :: String -> String -> Bool
findKey [] _ = True
findKey (x : xs) str = elem x str && findKey xs str

firstProperFactor :: [Int] -> Maybe Int
firstProperFactor [] = Nothing
firstProperFactor nums = 
  let
    num = last nums
    ns = init nums
  in L.find (\n -> n < num && num `mod` n == 0) ns  

areAllFactors :: [Int] -> [Int] -> Bool
areAllFactors factors numbers = 
  all (\n -> s `mod` n == 0) nums
  where
    s = sum numbers
    nums = map (numbers !!) factors

hasIncrementingList :: [Int] -> [Int] -> Bool
hasIncrementingList starts numbers = 
  any (\n -> L.isInfixOf [n, n+1, n+2] numbers) starts

hashUntilIncrement :: ByteString -> Int -> Int
hashUntilIncrement key initial = 
  case first3 (mkHash key initial) of
      Just triple -> if consecutive triple
                        then initial 
                        else hashUntilIncrement key (initial + 1)
      Nothing -> hashUntilIncrement key (initial + 1)
  where
    consecutive (x, y, z) = x + 1 == y && y + 1 == z

first3 :: ByteString -> Maybe (Word8, Word8, Word8)
first3 bs = case take 3 (BS.unpack bs) of
  cs@[c1, c2, c3] -> if all (\d -> d >= 48 && d <= 57) cs
    then Just (c1 - 48, c2 - 48, c3 - 48)
    else Nothing
  _ -> Nothing

mkHash :: ByteString -> Int -> ByteString
mkHash key i = finalize c2
  where
    c1 = start key
    c2 = update c1 (pack . show $ i)
