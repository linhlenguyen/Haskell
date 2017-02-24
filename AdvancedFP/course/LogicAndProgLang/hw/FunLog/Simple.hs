module Simple where

------------------------------
class Simple a where
  simple :: a -> String
  simpleList :: [a] -> String
  -- Default definition 
  simpleList xs = plistf simple "[" xs "," "]"
  
instance Simple Int where
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
  
  
plistf :: (a -> String) -> String -> [a] -> String -> String -> String
plistf f open xs sep close = open ++ help xs ++ close
  where help [] = ""
        help [x] = f x
        help (x:xs) = f x ++ sep ++ help xs    
  

plist :: Show a => String -> [a] -> String -> String -> String
plist = plistf show

