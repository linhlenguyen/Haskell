module Dimension where

import Auxfuns(plistf)
import Literal
import Data.List(elemIndex)

-- A Dimension is a finite set of some Base type.
-- Invariant that (Dim n b xs) => length xs == n
-- We store the elements of the set as a [Literal]
-- The order in the list assigns an Int (a rank) to each element.

data Dimension = Dim Int Base [Literal]

-- we can interpret a [Literal] of length n in another
-- sense, as an n-tuple from product of Dimensions
-- which we represent as [Dimension]

type Tuple = [Literal]  
type Indices = [ Int ]

-- A key assigns a unique integer to every Tuple
-- It is an invariant that the integer in a Key 
-- is monotonic with respect to the tuple.
-- i.e. Given (Key n xs) and (Key m ys),  if n < m then xs < ys
-- and if n==m then xs == ys. 

data Key = Key Int Indices Tuple
  deriving Show

tuple (Key n indx lits) = lits
flat (Key n indx lits) = n

-- Literals in a Dimension are ordered as they appear in the 
-- element list. Thus given a [Dimension] we can rank order 
-- every tuple, so that tuples are ordered lexographically

allTuples:: [Dimension] -> [Tuple]
allTuples [] = []
allTuples [d] = map (:[]) (dimElem d)
allTuples (d:ds) = [(x:xs) | x<- dimElem d, xs <- allTuples ds]

allIndicies:: [Dimension] -> [[Int]]
allIndicies [] = []
allIndicies [d] = map (:[]) (dimIndex d)
allIndicies (d:ds) = [(x:xs) | x <- dimIndex d, xs <- allIndicies ds]

dimIndex d = [0.. dimSize d - 1]

-- we can compute all the keys in a n-dimension space.

allKeys ds =zipWith3 Key [0..] (allIndicies ds)(allTuples ds)

-- Thus it makes sense to make the following instances

instance Eq Key where
  (Key n is xs) == (Key m js ys) = n==m
  
instance Ord Key where
  compare (Key n is xs) (Key m js ys) = compare n m

-- Sometimes we need to compute a key, given the
-- dimensions and a tuple, and vice-versa

toKey:: String -> [Dimension] -> Tuple -> Key
toKey s ds lits | length ds /= length lits = error ("Bad toKey "++s++" "++dimNameL ds++" "++show lits)
toKey s ds lits = Key (flatIndex ds indexes) indexes lits
    where indexes = (zipWith (litToIndex ("in toKey "++s)) ds lits)

fromKey :: Key -> Tuple
fromKey (Key n indx xs) = xs

litToIndex :: String -> Dimension -> Literal -> Int                        
litToIndex message (Dim n base xs) a =
   case elemIndex a xs of
     Just n -> n
     Nothing -> error ("The element: "++show a++" can't be found in the index set "++show xs++"\n"++message)

flatIndex:: [Dimension] -> [Int] -> Int
flatIndex dims indices = help 1 (reverse (map dimSize dims)) (reverse indices) 0
  where help m [] [] ans = ans
        help m (d:ds) (x:xs) ans = help (m*d) ds xs (m*x + ans)

-------------------------------------------------
-- When joining two relations we need to compare on
-- the first n-elements of the two tuples.

compareKey:: Int -> Key -> Key -> Ordering
compareKey n (Key _ is xs) (Key _ js ys) = help n is js
 where help 0 xs ys = EQ
       help 1 (x:xs) (y:ys) = compare x y
       help n (x:xs) (y:ys) | x==y = help (n-1) xs ys
                            | otherwise = compare x y
       help _ [] (y:ys) = LT
       help _ (x:xs) [] = GT
       help _ [] [] = EQ 

----------------------------------------------
-- When projecting we need to recompute a key
-- since some of the elements of the tuple are 
-- either reordered, duplicated, or removed.

keyProj:: [Dimension] -> [Int] -> Key -> Key
keyProj ds ps (Key n is xs) = (key)
  where js = map (is!!) ps
        key = Key (flatIndex ds2 js) js (map (xs!!) ps)
        ds2 = map (ds!!) ps

----------------------------------------------------------------------
-- Extracting information from a Dimension

dimSize (Dim n base ls) = n
dimElem (Dim n base ls) = ls

-- For printing parts of dimensions
dimName (Dim n base ls) =  show base++"#"++show n

dimNameAll (Dim n base (l:ls)) = show base++"#"++show(l:ls)

dimNameL ds = plistf dimName "(" ds "," ")"

showDimSize (Dim n base xs) = show n

showDimL ds = show(map dimName ds)

instance Show Dimension where
  show = dimNameAll
  
---------------------------------------------------------
-- We can create Dimensions a number of ways.
-- State the size, and we generate a mapping [0 .. n]

dim :: Int -> Dimension
dim n = Dim n Int [LInt x | x <- [0 .. n-1]]

-- Use a list of strings to describe a Dimension
dimS:: [String] -> Dimension
dimS xs = Dim (length xs) String (map LString xs)

-- Exhibit a list of anything that can be turned into a string.
dimL :: (Show a) => [a] -> Dimension
dimL xs = Dim (length xs) String (map (LString . show) xs)

-- Any element of a type in the Enum class describes a dimension
-- over the finite set described by the full enumeration.
dimE :: (Show a,Enum a) => String -> a -> Dimension
dimE name x = Dim (length xs) (Enum name) (map (LCon name . show) xs)
  where first = (toEnum 0)
        theyHaveTheSameType = [first,x]
        xs = enumFrom first

-- Create a multi-Dimensional mapping where all the individual 
-- mappings are over a 0-based range of Int.

expand :: [Int] -> [Dimension]
expand xs = map dim xs

---------------------------------
sze = Dim 3 Int (map LInt [2,1,0])
nme = Dim 3 String (map LString ["bob","tim","tom"])
lc = Dim 2 String (map LString ["a","b"])
ds = [dimS ["b","a"] ,dimS ["y","x","z"],dim 3]

