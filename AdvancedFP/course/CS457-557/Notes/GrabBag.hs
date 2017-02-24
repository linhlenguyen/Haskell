module GrabBag where
import System.IO
--import System.IO.Error
import Control.Exception
import Data.Array

data Tree a = Leaf a 
            | Branch (Tree a) (Tree a)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch x y) = 
        Branch (fmap f x) (fmap f y)

{-
-- These instance are defined in libraries

instance Functor ((,) c) where
   fmap f (x,y) = (x, f y)

   
instance Functor [] where
    fmap f []     = []
    fmap f (x:xs) = f x : fmap f xs

instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)
   
-}

---------------------------------------------------
data Tree2 t a = Tip a 
              | Node (t (Tree2 t a))

t1 :: Tree2 [] Int
t1 = Node [Tip 3, Tip 0]
 

data Bin x = Two x x

t2 :: Tree2 Bin Int
t2 = Node (Two(Tip 5) (Tip 21))

{- 
instance Functor (Tree2 Bin) where
  fmap f (Tip x) = Tip(f x)
  fmap f (Node (Two x y)) = Node (Two (fmap f x) (fmap f y))

instance Functor (Tree2 []) where
  fmap f (Tip x) = Tip(f x)
  fmap f (Node xs) = Node (map (fmap f) xs)
-}

instance Functor t => Functor (Tree2 t) where
  fmap f (Tip x) = Tip(f x)
  fmap f (Node xs) = Node (fmap (fmap f) xs)  
  
---------------------------------------------

catchIO :: IO a -> (IOError -> IO a) -> IO a
catchIO = catch

getAndOpenFile :: String -> IOMode -> IO Handle
getAndOpenFile prompt mode =
  do { putStr prompt
     ; name <- getLine
     ; catchIO (openFile name mode)
             (\e -> do { putStrLn 
                          ("Cannot open: "++name)
                       ; print e
                       ; getAndOpenFile prompt mode
                       })
     }
 
 
main =
  do { fromHandle <- getAndOpenFile 
                      "Copy from: " ReadMode
     ; toHandle <- getAndOpenFile 
                      "Copy to: " WriteMode
     ; contents <- hGetContents fromHandle
     ; hPutStr toHandle contents
     ; hClose fromHandle
     ; hClose toHandle
     ; putStr "Done\n"
     }

-------------------------------------
-- arrays

-- class Ord a => Ix a where
--   range :: (a, a) -> [a]
--   index :: (a, a) -> a -> Int
--   inRange :: (a, a) -> a -> Bool
--   rangeSize :: (a, a) -> Int


data Color = Red | Blue | Green | Yellow | White | Black
  deriving (Ord,Eq,Ix,Show)

alphabet = array (1,26) 
             (zip [1..26] "abcdefghijklmnopqrstuvwxyz")

fifth = alphabet ! 5 

digits = listArray (0,9) "0123456789"

as = elems alphabet
bs = assocs alphabet