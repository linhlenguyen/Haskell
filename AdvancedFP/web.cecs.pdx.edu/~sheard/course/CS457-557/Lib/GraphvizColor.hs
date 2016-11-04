module Graphviz(Tree(..),LabeledTree(..),Color(..),Shape(..),toPng,Graph(..),
                shape,pnG,pnGs,pdfG) where
import System.Cmd(system)
import System.Info(os)

-- FIXME: We really need a Style type that is more flexible but this was easy and was 
-- enough to implement good DFA animations. Also it is source compatible with the old 
-- version which is nice. Feel free to change this.
data Color = None | Black | Red | Blue | Green
           | LightGray | Gray 
           | Filled Color Color deriving(Show, Eq)
           
data Shape = Box | Circle | Oval | Doublecircle   

showShape Box = "box"
showShape Oval = "oval"
showShape Circle = "circle"
showShape Doublecircle = "doublecircle"

showColor None = ""
showColor c = "["++colorAttr c++"]"

colorName Black = "black"
colorName Red = "red"
colorName Blue = "blue"
colorName Green = "darkgreen"
colorName Gray = "gray"
colorName LightGray = "lightgray"

colorAttr (Filled edge fill) = case (edge, fill) of
                                 (None, None) -> ""
                                 (None, _) -> filledStyle ++ fillAttr
                                 (_, None) -> edgeAttr
                                 (_, _) -> edgeAttr ++ "," ++ filledStyle ++ fillAttr 
      where edgeAttr = if edge /= None then "color=" ++ (colorName edge) else ""
            fillAttr = if fill /= None then "fillcolor=" ++ (colorName fill) else ""
            filledStyle = "style=filled,"
colorAttr c = "color=" ++ (colorName c) 

shape state finalStates = if elem state finalStates then Doublecircle else Oval

-----------------------------------------
-- Class definitions

class Tree t where
  subtrees :: t -> [(t,Color)]
  
class Tree t => LabeledTree t where
  label :: t -> String

---------------------------------------------
-- Overloaded functions

depth  :: Tree t => t -> Int
depth   = (1+) . foldl max 0 . map depth . (map fst) . subtrees

size   :: Tree t => t -> Int
size    = (1+) . sum . map size . (map fst) . subtrees

paths               :: Tree t => t -> [[t]]
paths t | null br    = [ [t] ]
        | otherwise  = [ t:p | b <- br, p <- paths b ]
          where br = map fst (subtrees t)

dfs    :: Tree t => t -> [t]
dfs t   = t : concat (map dfs (map fst(subtrees t))) 


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
  subtrees (l :^: r)  = [(l,Red),(r,Green)]
  
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
 --    ; system "dot -Tpdf tree.dot > /u/hook/public_html/tree.pdf"
      ; system "dot -Tpng tree.dot > tree.png "
      ; system "explorer tree.png "
     }
     

type Path    = [Int]
type NodeId  = String

showPath      :: Path -> String
showPath p     = "\"" ++ show p ++ "\""

nodeTree    :: LabeledTree t => Path -> t -> [String]
nodeTree p t = [ showPath p ++ " [label=\"" ++ label t ++ "\"]" ]
            ++ concat (zipWith (edgeTree p) [1..] (subtrees t))

edgeTree      :: LabeledTree t => Path -> Int -> (t,Color) -> [String]
edgeTree p n (t,color) = [ showPath p ++ " ->" ++ showPath p' ++ showColor color ] ++ nodeTree p' t
                 where p' = n : p
                 
--------------------------------------------------------
-- Pictures of Graph like things

class Graph t where
  nodes:: t -> [(Int,String,Color,Shape)]
  edges:: t -> [(Int,Int,String,Color)]
  graphStyle :: t -> String
  graphStyle _ = ""
  
showNode (n,str,col,shape) = show n++showAttr (str,col,Just shape)++";"
showEdge (n,m,str,col) = show n++" -> "++show m++showAttr(str,col,Nothing)++";"

showAttr (str,None,Nothing) = " [label="++show str++"]"
showAttr (str,None,Just sh)  = " [label="++show str++", shape="++showShape sh++"]"
showAttr (str,c,Just sh) = " [label="++show str++", "++colorAttr c++", shape="++showShape sh++"]"


dotG:: Graph t => t -> String
dotG t = (unlines lines)
   where ns = nodes t
         es = edges t
         gs = graphStyle t
         lines = ["digraph tree {"]++map showNode ns++map showEdge es++["",gs,"}"]


pnG t =
  do { writeFile "graph.dot" (dotG t)
     ; case os of
        "mingw32" -> system "dot -Tpng graph.dot > graph.png "      -- for Windows, Mac, Linux
        other -> error ("Add a case for a new operating system in function pnG, in file Graphviz.hs")
        -- system "dot -Tpdf graph.dot > /u/hook/public_html/graph.pdf "  -- for Unix
     ; case os of
        "mingw32" -> system "explorer graph.png "  -- Windows
        other ->  error ("Add another case for a new operating system in function pnG, in file Graphviz.hs")
                 -- system "eog graph.png "       -- Linux/Gnome
                 -- system "open graph.png "      -- Mac
     }

pdfG f t = 
    do { writeFile (f++".dot") (dotG t)
       ; system $ "dot -Tpdf "++f++".dot > "++f++".pdf "
       }


pnGs s t =
  do { writeFile (s++".dot") (dotG t)
     ; system ("dot -Tpng "++s++".dot > "++s++".png ") -- for Windows, Mac, Linux
  -- ; system ("dot -Tpdf "++s++".dot > /u/hook/public_html/"++s++".pdf ")  -- for Unix
  -- ; system ("explorer "++s++".png ")  -- Windows
     ; system ("eog "++s++".png ")       -- Linux/Gnome
 --  ; system ("open "s++".png ")     -- Mac
     }