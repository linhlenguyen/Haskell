--Haskell story building

--Once there was a
--Noun populator here!

--Object with associated tag 
--Data structure

--Array of tag
--Has to be unique, not case sensitive
data Tag = Tag String

instance Show Tag where
Tag a

hasTag :: (Tag a) => a -> [a] -> Bool
hasTag (Tag a) [] = False
hasTag (Tag a) (x:xs)
| a == x : True
| otherwise = addTag a xs

addTag :: (Tag a) => a -> [a] -> [a]
addTag (Tag a) [] = [a]
addTag (Tag a) x = if hasTag a x then x else a : x

--Supporting types
Either a b = Left a | Right b deriving (Show, Read, Eq, Ord)
