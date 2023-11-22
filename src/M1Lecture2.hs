module M1Lecture2 where

import Crypto.Hash.MD5 (start, update, finalize)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import qualified Data.List as L
import Data.Word (Word8, Word)

parallelCars :: (Word, Word) -> (Word, Word) -> Bool
parallelCars (x1, v1) (x2, v2) = undefined

findKey :: String -> String -> Bool
findKey = undefined

firstProperFactor :: [Int] -> Maybe Int
firstProperFactor = undefined

areAllFactors :: [Int] -> [Int] -> Bool
areAllFactors factors numbers = undefined

hasIncrementingList :: [Int] -> [Int] -> Bool
hasIncrementingList starts numbers = undefined

hashUntilIncrement :: ByteString -> Int -> Int
hashUntilIncrement key initial = undefined

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
