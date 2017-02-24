
There are many different kinds of Tree data structure, including the
following binary tree data type that stores values only at leaf nodes:

> data BinTree a   = Leaf a
>                  | BinTree a :^: BinTree a
>                    deriving Show

We'll talk more about the "deriving Show" part of this declaration in
class soon, but for now you can just treat it as an indication that
we want to be able to print out values of the tree type.  For example,
we can define the following tree:

> example :: BinTree Int
> example  = l :^: r
>  where l = p :^: q
>        r = s :^: t
>        p = Leaf 1 :^: t
>        q = s :^: Leaf 2
>        s = Leaf 3 :^: Leaf 4
>        t = Leaf 5 :^: Leaf 6

And then print it out at the Hugs/GHCi prompt:

  Main> example
  ((Leaf 1 :^: (Leaf 5 :^: Leaf 6)) :^: ((Leaf 3 :^: Leaf 4) :^: Leaf 2))
  :^: ((Leaf 3 :^: Leaf 4) :^: (Leaf 5 :^: Leaf 6))
  Main>

Not exactly pretty, but let's proceed.  For example, we can define
a mapTree function that applies a function to each of the leaf values
in a tree:

> mapTree            :: (a -> b) -> BinTree a -> BinTree b
> mapTree f (Leaf x)  = Leaf (f x)
> mapTree f (l :^: r) = mapTree f l :^: mapTree f r

Now we can convert our example tree into a value of type BinTree String
as follows:

  Main> mapTree show example
  ((Leaf "1" :^: (Leaf "5" :^: Leaf "6")) :^: ((Leaf "3" :^: Leaf "4")
  :^: Leaf "2")) :^: ((Leaf "3" :^: Leaf "4") :^: (Leaf "5" :^: Leaf "6"))
  Main>

Or we can add one to every number in a BinTree Int as follows:

  Main> mapTree (1+) example
  ((Leaf 2 :^: (Leaf 6 :^: Leaf 7)) :^: ((Leaf 4 :^: Leaf 5) :^: Leaf 3))
  :^: ((Leaf 4 :^: Leaf 5) :^: (Leaf 6 :^: Leaf 7))
  Main>

Still not pretty.  Wouldn't it be nice if we could see these trees in a
more graphical form?

Dot is a language for describing tree/graph structures.  For example,
we could describe the tree for (Leaf "a" :^: Leaf "b" :^: Leaf "c") as
follows:

  digraph tree {
    "1" [label=""];
    "1" -> "2";
    "2" [label=""];
    "2" -> "3";
    "3" [label="a"];
    "2" -> "4";
    "4" [label="b"];
    "1" -> "5";
    "5" [label="c"];
  }

General form: digraph name { stmts } where each stmt is either

- node_id [label="text"];
  constructs a node with the specified id and label.

- node_id -> node_id;
  constructs an edge between the specified pair of nodes.

The Graphviz toolset can be used to convert a dot description into
a range of different graphical formats.  For example:

  dot -Tpng abc.dot > abc.png

How can we turn a BinTree into a dot file?  For simplicity, let's
assume that the values stored in the BinTree (i.e., the values that
we want to use as labels) are Strings.  What should we use as node
identifiers?

Answer: paths.

- The root node of a tree has path []

- The nth child of the node with path p has path (n : p)

> type Path    = [Int]
> type NodeId  = String

We can convert a path into a NodeId as follows (we add a pair
of double quotes around the string to avoid confusing the Graphviz
tools:

> showPath      :: Path -> String
> showPath p     = "\"" ++ show p ++ "\""

Next problem: How can we capture the essential structure of a
tree?  Two utility functions:

> subtrees           :: BinTree a -> [BinTree a]
> subtrees (Leaf x)   = []
> subtrees (l :^: r)  = [l, r]

> nodeLabel          :: BinTree String -> String
> nodeLabel (Leaf x)  = x
> nodeLabel (l :^: r) = ""

Now we can generate a list of dot statements for a tree t at
path p using the following code:

> nodeTree    :: Path -> BinTree String -> [String]
> nodeTree p t = [ showPath p ++ " [label=\"" ++ nodeLabel t ++ "\"]" ]
>             ++ concat (zipWith (edgeTree p) [1..] (subtrees t))

> edgeTree      :: Path -> Int -> BinTree String -> [String]
> edgeTree p n c = [ showPath p ++ " -> " ++ showPath p' ] ++ nodeTree p' c
>                  where p' = n : p

Wrapping this in a little header and inserting semicolons after
each statement will give us a complete dot description of a BinTree:

> toDot :: BinTree String -> IO ()
> toDot t = writeFile "tree.dot"
>            ("digraph tree {\n"
>             ++ semi (nodeTree [] t)
>             ++ "}\n")
>  where semi = foldr (\l ls -> l ++ ";\n" ++ ls) ""

All well and good, but what about all the other kinds of trees that
we might want to work with?

> data LabTree l a = Tip a
>                  | LFork l (LabTree l a) (LabTree l a)

> data STree a     = Empty
>                  | Split a (STree a) (STree a)

> data RoseTree a  = Node a [RoseTree a]

> data Expr        = Var String
>                  | IntLit Int
>                  | Plus Expr Expr
>                  | Mult Expr Expr

It seems pretty obvious that we could repeat the above process
for each of these different tree types to get dot versions of
the trees in each case ... but is there a better way to do this
than copy and paste?

Higher order functions:
-----------------------
The essential structure of BinTrees in the code above is captured
by the nodeLabel and subtrees functions.  What if we were to abstract
these out as parameters:

> nodeTree'    :: (t -> String) ->
>                  (t -> [t]) ->
>                   Path -> t -> [String]

> edgeTree'    :: (t -> String) ->
>                  (t -> [t]) ->
>                   Path -> Int -> t -> [String]

> nodeTree' lab sub p t
>   = [ showPath p ++ " [label=\"" ++ lab t ++ "\"]" ]
>     ++ concat (zipWith (edgeTree' lab sub p) [1..] (sub t))

> edgeTree' lab sub p n c
>   = [ showPath p ++ " -> " ++ showPath p' ] ++ nodeTree' lab sub p' c
>     where p' = n : p

Wrapping this in a little header and inserting semicolons after
each statement will give us a complete dot description of a BinTree:

> toDot' :: (t -> String) ->
>            (t -> [t]) ->
>             t -> IO ()
> toDot' lab sub t
>         = writeFile "tree.dot"
>            ("digraph tree {\n"
>             ++ semi (nodeTree' lab sub [] t)
>             ++ "}\n")
>  where semi = foldr (\l ls -> l ++ ";\n" ++ ls) ""

Now we can customize toDot' to work for any tree type that we
like:



> toDotLabTree = toDot' lab sub
>  where lab (Tip a)       = a
>        lab (LFork s l r) = s
>        sub (Tip a)       = []
>        sub (LFork s l r) = [l, r]

> toDotSTree = toDot' lab sub
>  where lab Empty = ""
>        lab (Split s l r) = s
>        sub Empty = []
>        sub (Split s l r) = [l, r]

> toDotRoseTree = toDot' lab sub
>  where lab (Node x cs) = x
>        sub (Node x cs) = cs

> toDotExpr = toDot' lab sub
>  where lab (Var s)    = s
>        lab (IntLit n) = show n
>        lab (Plus l r) = "+"
>        lab (Mult l r) = "*"
>        sub (Var s)    = []
>        sub (IntLit n) = []
>        sub (Plus l r) = [l, r]
>        sub (Mult l r) = [l, r]

toDotExpr (Plus (Mult (Var "x") (IntLit 3)) (Mult (Var "y") (IntLit 5)))

Ok, so this works, it is more general (because we can now generate
dot versions for multiple tree types), and we get some reuse too.

> toDot'' :: (t -> String) -> (t -> [t]) -> t -> IO ()
> toDot'' lab sub t
>  = writeFile "tree.dot"
>      ("digraph tree {\n" ++ semi (nodeTree' [] t) ++ "}\n")
>  where
>   semi = foldr (\l ls -> l ++ ";\n" ++ ls) ""
>   nodeTree' p t
>     = [ showPath p ++ " [label=\"" ++ lab t ++ "\"]" ]
>     ++ concat (zipWith (edgeTree' p) [1..] (sub t))
>   edgeTree' p n c
>     = [ showPath p ++ " -> " ++ showPath p' ] ++ nodeTree' p' c
>       where p' = n : p

But it's still pretty ugly!

Now let's switch to treedot2 to see how this can be done with type
classes ...

> go1 = toDot (mapTree show example)

