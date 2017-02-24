module Graphviz where
import System.Cmd(system)

-----------------------------------------
-- Class definitions

class Tree t where
  subtrees :: t -> [t]
  
class Tree t => LabeledTree t where
  label :: t -> String

---------------------------------------------
-- Overloaded functions

depth  :: Tree t => t -> Int
depth   = (1+) . foldl max 0 . map depth . subtrees

size   :: Tree t => t -> Int
size    = (1+) . sum . map size . subtrees

paths               :: Tree t => t -> [[t]]
paths t | null br    = [ [t] ]
        | otherwise  = [ t:p | b <- br, p <- paths b ]
          where br = subtrees t

dfs    :: Tree t => t -> [t]
dfs t   = t : concat (map dfs (subtrees t)) 


-----------------------------------------------------
-- An example with instances

data BinTree a   = Leaf a
                 | BinTree a :^: BinTree a
                   deriving Show

example :: BinTree Int
example  = l :^: r
 where l = p :^: q
       r = s :^: t
       p = Leaf 1 :^: t
       q = s :^: Leaf 2
       s = Leaf 3 :^: Leaf 4
       t = Leaf 5 :^: Leaf 6

instance Tree (BinTree a) where
  subtrees (Leaf x)   = []
  subtrees (l :^: r)  = [l, r]
  
instance Show a => LabeledTree (BinTree a) where
  label (Leaf x)   = show x
  label (l :^: r)  = ""

instance Functor BinTree where
  fmap f (Leaf x)   = Leaf (f x)
  fmap f (l :^: r)  = fmap f l :^: fmap f r


-----------------------------------------------------------
-- Code for using Graphviz to draw trees

toDot :: LabeledTree t => t -> IO ()
toDot t = writeFile "tree.dot"
           ("digraph tree {\n"
            ++ semi (nodeTree [] t)
            ++ "}\n")
 where semi = foldr (\l ls -> l ++ ";\n" ++ ls) ""

toPng t =
  do { toDot t
     ; system "dot -Tpng tree.dot > tree.png"
     -- ; system "\"d:/Programs/Mozilla Firefox/firefox.exe\" tree.png "
     }

type Path    = [Int]
type NodeId  = String

showPath      :: Path -> String
showPath p     = "\"" ++ show p ++ "\""

nodeTree    :: LabeledTree t => Path -> t -> [String]
nodeTree p t = [ showPath p ++ " [label=\"" ++ label t ++ "\"]" ]
            ++ concat (zipWith (edgeTree p) [1..] (subtrees t))

edgeTree      :: LabeledTree t => Path -> Int -> t -> [String]
edgeTree p n c = [ showPath p ++ " ->" ++ showPath p' ] ++ nodeTree p' c
                 where p' = n : p
