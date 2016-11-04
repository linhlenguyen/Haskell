-------------------------------------------------------------------------------
A simple interactive tree editor              Mark P Jones,  September 19, 1996
================================              =================================

Part 1: Forests and trees
-------------------------
We use standard datatypes to represent forests of so-called general trees:

> type Forest a = [Node a]           -- A Forest is a list of nodes, each of
> data Node a   = Node a (Forest a)  -- which has a value and some children.

Here is a simple example:

> myForest     :: Forest String
> myForest      = [Node "1"
>                    [Node "1.1"
>                       [Node "1.1.1" [],Node "1.1.2"[]],
>                     Node "1.2" []],
>                  Node "2" []]

> t1 = putStrLn(showForest myForest)

Next we define two simple, general purpose operations for working with
forests.  First, forestElems, which enumerates the values in a forest in
depth first order:

> forestElems  :: Forest a -> [a]
> forestElems   = concat . map nodeElems
>  where nodeElems (Node x cs) = x : forestElems cs

The second function is depthMap, which traverses a forest and creates a
new one of the same shape by applying a function that takes an extra
parameter supplying the depth of the tree at that point:

> depthMap     :: (Int -> a -> b) -> Int -> Forest a -> Forest b
> depthMap f d  = map depthNode
>  where depthNode (Node x cs) = Node (f d x) (depthMap f (d+1) cs)

These functions can be used to help display the structure of a forest:

> showForest   :: Forest String -> String
> showForest    = unlines . forestElems . depthMap indent 1
>  where indent d x = replicate (2*d) ' ' ++ x


Part 2: Navigation
------------------
For the purposes of navigation, we need to have a way of describing
positions within a forest.  For any position, we need to capture:

 o The nodes to the left of the current position (which we will keep in
   a list with rightmost element first, that is, in reverse order).

 o The nodes to the right of the current position, also in a list.

 o A sequence of levels up the tree, from the current position to the
   root.  We need to know the position within each level, which we
   represent by a triple (left, x, right) where left and right are the
   siblings on either side, and x is the value of the dominating node.
      
This leads naturally to the following datatype definition:

> data Position a = Pos {left::[Node a], up::[Level a], right::[Node a]}
> type Level a    = ([Node a], a, [Node a])

It is fairly easy to convert between forests and positions (although the
position information is lost, because there can be many different positions
within a given forest):

> rootPosition   :: Forest a -> Position a
> rootPosition f  = Pos [] [] f

> reconstruct               :: Position a -> Forest a
> reconstruct (Pos ls us rs) = foldl recon (reverse ls ++ rs) us
>  where recon fs (ls,x,rs) = reverse ls ++ [Node x fs] ++ rs

The following function finds the value (if any) associated
with the node on the immediate right of current position:

> rightValue                         :: Position a -> Maybe a
> rightValue (Pos _ _ (Node x _ : _)) = Just x
> rightValue _                        = Nothing

There are four functions for moving around in a forest, either to the left,
to the right, up, or down.  In the last case, there are two possibilities:
down the tree on the immediate left of the current position, or down the
tree on the immediate right.  For simplicity, we will only consider the
latter.  All of these functions could fail if the requested move is not
possible, so the resulting position is returned in a Maybe type.

> moveUp, moveDown, moveLeft, moveRight :: Position a -> Maybe (Position a)

> moveLeft  (Pos ls us rs)
>                = repos ls (\n ns -> Pos ns us (n:rs))

> moveRight (Pos ls us rs)
>                = repos rs (\n ns -> Pos (n:ls) us ns)

> moveDown  (Pos ls us rs)
>                = repos rs (\(Node x cs) ns -> Pos [] ((ls,x,ns):us) cs)

> moveUp    (Pos ls us rs)
>                = repos us (\(as,x,bs) vs -> Pos as vs (make x : bs))
>                  where make x = Node x (reverse ls ++ rs)

Each of these functions works by inspecting a list and taking some action
if it is non-empty  -- which signals that a move is possible.  We capture
this general pattern in the following repositioning function:

> repos         :: [b] -> (b -> [b] -> Position a) -> Maybe (Position a)
> repos []     f = Nothing
> repos (x:xs) f = Just (f x xs)

We will also want simple methods for inserting and deleting nodes to the
right of the current position (we won't bother with the obvious duals for
insertion or deletion on the left).

> insertNode    :: a -> Position a -> Position a
> insertNode x (Pos ls us rs)
>                = Pos ls us (Node x [] : rs)

> deleteNode    :: Position a -> Maybe (Position a)
> deleteNode (Pos ls us rs)
>                = repos rs (\_ ns -> Pos ls us ns)

As a mildly amusing little extension, we can define a reflect operator:

> reflect       :: Position a -> Position a
> reflect (Pos ls us rs) = Pos rs us ls

This could have been used to define moveLeft in terms of moveRight (or
vice versa).


Part 3: User interface
----------------------
The main interactive process is defined as follows:

> type Pos = Position String

> mip  :: Pos -> IO ()
> mip p = do ch <- getChar
>            putChar '\n'
>            case ch of
>             '\n' -> mip p                              -- whitespace
>             '\t' -> mip p
>             ' '  -> mip p
>                                                        -- basic movement
>             'f'  -> tryTo p moveDown  noNode mip       -- F irst child    
>             'n'  -> tryTo p moveRight noNode mip       -- N ext sibling
>             'b'  -> tryTo p moveLeft  noPrev mip       -- B ack to previous sibling
>             'p'  -> tryTo p moveUp    noPar  mip       -- P arent
> 
>             'd'  -> tryTo p deleteNode noNode mip      -- delete and insert
>
>             'i'  -> do key <- getLine                  -- use as: i<label>\n
>                        mip (insertNode key p)
>
>             'k'  -> tryTo p rightValue noNode $ \x ->  -- display commands
>                     putStrLn x >> mip p
>
>             -- The method used to display the tree differs from the original
>             -- program, and displays the entire tree structure, using a <*>
>             -- symbol to mark the current position.
>             's'  -> do putStr (showForest (reconstruct (insertNode "<*>" p)))
>                        mip p
>
>             'r'  -> mip (reflect p)                    -- a reflection
>
>             'q'  -> return ()                          -- quit command
>
>             _    -> do putStrLn "Error: unrecognized command"
>                        mip p

This definition has been simplified by abstracting out a common pattern for
dealing with the results of Maybe types:

> -- tryTo         :: Pos -> (Pos -> Maybe Pos) -> String -> (Pos -> IO ()) -> IO ()
> tryTo p f e c  = case f p of
>                    Nothing -> putStrLn e >> mip p
>                    Just x  -> do -- putStr (showForest (reconstruct (insertNode "<*>" x)))
>                                  c x

I have also abstracted out the strings produced by some of the error messages:

> noNode = "Error: not at node"
> noPrev = "Error: no previous sibling"
> noPar  = "Error: node has no parent"

A simple program that starts up the tree editor at the root of myForest can
be defined as follows:

> main = mip (rootPosition myForest)

-------------------------------------------------------------------------------
