module HashFunction where

import Data.Int
import Char(ord)
import ArrCommands

-- A prime larger than the maximum hash table size 
prime:: Int
prime = 1500007

-- Sample hash function for Int
hashInt :: Int -> Int32
hashInt x = (fromIntegral x) `mod` (fromIntegral prime)

-- Sample hash function for String
hashString :: String -> Int32
hashString = fromIntegral . foldr f 0
      where f c m = ord c + (m * 128) `rem` prime

{-
Good properties of Hash functions (wikipedia)
1) low cost        (cheap to compute)
2) Determinism     (repeatedly get the same result)
3) Uniformity      (even distribution)
4) Variable range  (adjusts to more than one range, usually table size)
-}

hashI:: Int -> Int -> Int
hashI x range = fromIntegral(hashInt x) `mod` range

hashS:: String -> Int -> Int
hashS x range = fromIntegral(hashString x) `mod` range

---------------------------------------------------

-- hashFind :: Eq key => key -> Array [(key,value)] -> IO(Maybe value)
hashFind :: String -> Array [(String,value)] -> IO(Maybe value)
hashFind key arr = 
  do { (lo,hi) <- boundsArr arr
     ; let bucketNumber = hashS key (((hi -lo)+1) + lo)
     ; possible <- readArr arr bucketNumber
     ; return(lookup key possible)
     }
