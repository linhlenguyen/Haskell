
module Auxfuns where

import qualified Text.PrettyPrint.HughesPJ as PP
 
-- an attempt to print out simple types without annoying quotes.

class Simple a where
  simple :: a -> String
  simpleList :: [a] -> String
  -- Default definition 
  simpleList xs = plistf simple "[" xs "," "]"
  
instance Simple Int where
  simple n = show n
instance Simple Integer where
  simple n = show n  
instance Simple Char where
  simple c =[c]
  simpleList xs = xs
instance Simple Bool where
  simple b = show b
instance (Simple a, Simple b) => Simple (a,b) where
  simple (x,y)  = "("++simple x++","++simple y++")"
instance (Simple a, Simple b,Simple c) => Simple (a,b,c) where
  simple (x,y,z)  = "("++simple x++","++simple y++","++simple z++")"  
instance Simple a => Simple [a] where
  simple xs = simpleList xs
  
-- To print lists with separators and open and close brackets

plistf :: (a -> String) -> String -> [a] -> String -> String -> String
plistf f open xs sep close = open ++ help xs ++ close
  where help [] = ""
        help [x] = f x
        help (x:xs) = f x ++ sep ++ help xs    
        

plist :: Show a => String -> [a] -> String -> String -> String
plist = plistf show


class PP a where
  pp :: a -> PP.Doc

instance PP Bool where
  pp True = PP.text "True"
  pp False = PP.text "False"


------------------------
-- Now some common monadic functions

foldlM f base [] = return base
foldlM f base (x:xs) = do { b2 <- f x base; foldlM f b2 xs }

foldrM acc base [] = return base
foldrM acc base (x:xs) = 
  do { base2 <- foldrM acc base xs
     ; acc x base2 }

ifM b x y = do { z <- b; if z then x else y}

maybeM Nothing x y = x
maybeM (Just a) x y = y a

lift1M f comp = do { x <- comp; return(f x)}

whenM True string = error string
whenM False string = return ()