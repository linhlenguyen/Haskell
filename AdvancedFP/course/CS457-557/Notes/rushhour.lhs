This is a solver for Rush Hour puzzles, written in Haskell to demonstrate
uses of lazy evaluation.  Mark P Jones, February 2007

                             * * * * * *

> import List

Rush Hour:  "Escape!  That's the goal.  Rush Hour is a premier sliding
block puzzle designed to challenge your sequential-thinking skills (and
perhaps your traffic-officer aspirations as well)."

                             * * * * * *

A representation for the board:

We assume that the board is maxw squares wide and maxh squares high, and
that individual squares on the board is described by a pair of
coordinates (x,y) where x is in the range 1..maxw and y is in the range
1..maxh.

> type Position = (Coord, Coord)
> type Coord    = Int

> maxw, maxh   :: Coord
> maxw          = 6
> maxh          = 6


A representation for the vehicles:

We also assume that there is some fixed collection of playing pieces
(not all of which are used in every puzzle).

> type Vehicle = (Color, Type)

> data Color   = Red  | LtGreen | Orange | LtBlue
>              | Pink | Purple  | Green  | Black
>              | Sand | Yellow  | Brown  | Olive
>              | Gold | Violet  | Blue   | Emerald
>                deriving (Eq, Show)

> data Type    = Car | Truck
>                deriving (Eq, Show)

> len         :: Type -> Int
> len Car      = 2
> len Truck    = 3


A representation for puzzles:

> type Puzzle      = [Piece]
> type Piece       = (Vehicle, Position, Orientation)
> data Orientation = W | H
>                    deriving (Eq, Show)

> vehicle :: Piece -> Vehicle
> vehicle (v, p, o) = v


Does this piece signal a solved puzzle?

> solved  :: Piece -> Bool
> solved p = p == ((Red, Car), (4,3), W)


Some simple puzzles:

> puzzle0 :: Puzzle
> puzzle0  = [ ((Red, Car),       (2,3), W), ((Yellow, Truck),  (5,3), H) ]

> puzzle1 :: Puzzle
> puzzle1  = [ ((LtGreen, Car),   (0,5), W), ((Yellow, Truck),  (5,3), H),
>              ((Violet, Truck),  (0,2), H), ((Blue, Truck),    (3,2), H),
>              ((Red, Car),       (1,3), W), ((Orange, Car),    (0,0), H),
>              ((LtBlue, Car),    (4,1), W), ((Emerald, Truck), (2,0), W) ]

> puzzle2 :: Puzzle
> puzzle2  = [ ((LtGreen, Car),   (0,4), H), ((Yellow, Truck),  (3, 5), W),
>              ((Red, Car),       (0,3), W), ((Orange, Car),    (3,3),  H),
>              ((Violet, Truck),  (5,2), H), ((LtBlue, Car),    (4,2),  H),
>              ((Purple, Car),    (4,1), W), ((Blue, Truck),    (0,2),  W),
>              ((Green, Car),     (0,0), W), ((Pink, Car),      (2,0),  H),
>              ((Olive, Car),     (3,0), W) ]

Does a specified piece obstruct a given square?

> puzzleObstructs           :: Puzzle -> Position -> Bool
> puzzleObstructs puzzle pos = or [ pieceObstructs p pos | p<-puzzle ]

> pieceObstructs                        :: Piece -> Position -> Bool
> pieceObstructs ((c,t), (x,y), W) (u,v) = (y==v) && (x<=u) && (u<x+len t)
> pieceObstructs ((c,t), (x,y), H) (u,v) = (x==u) && (y<=v) && (v<y+len t)


Calculate the legal moves for a given piece:

> moves             :: Puzzle -> Piece -> [Piece]
> moves puzzle piece = step back piece ++ step forw piece
>  where
>   free = not . puzzleObstructs puzzle
> 
>   back (v, (x,y), W) | x>0 && free p = Just (v, p, W)
>                        where p = (x-1, y)
>   back (v, (x,y), H) | y>0 && free p = Just (v, p, H)
>                        where p = (x, y-1)
>   back _                             = Nothing
>
>   forw (v, (x,y), W)
>            | x'<maxw && free (x',y) = Just (v, (x+1, y), W)
>              where x' = x + len (snd v)
>   forw (v, (x,y), H)
>            | y'<maxh && free (x,y') = Just (v, (x, y+1), H)
>              where y' = y + len (snd v)
>   forw _                             = Nothing
>
>   step      :: (a -> Maybe a) -> a -> [a]
>   step dir p = case dir p of
>                  Nothing -> []
>                  Just p' -> p' : step dir p'

Forest and trees:

> type Forest a = [Tree a]
> data Tree a   = Node a [Tree a]
>                 deriving Show

Mapping over trees:

> mapTree              :: (a -> b) -> Tree a -> Tree b
> mapTree f (Node x cs) = Node (f x) (map (mapTree f) cs)

Generating a forest of all possible moves:

> forest       :: Puzzle -> Forest (Piece, Puzzle)
> forest ps     = [ Node (m, qs) (forest qs)
>                    | (as, p, bs) <- splits ps,
>                      m <- moves (as++bs) p,
>                      let qs = as ++ [m] ++ bs ]

> splits       :: [a] -> [([a], a, [a])]
> splits []     = []
> splits (a:as) = [([], a, as)] ++ [ (a:us, x, vs) | (us,x,vs) <- splits as]

> splitz xs = zip3 (inits xs) xs (tail (tails xs))

Removing repeated moves:

> trimRel :: (a -> a -> Bool) -> Tree a -> Tree a
> trimRel rel (Node x cs) = Node x (filter (\(Node y _) -> rel x y) cs)


Removing duplicates

> trimDups :: Eq b => (a -> b) -> Forest a -> Forest a
> trimDups val f = f'
>  where
>   (f', xss) = prune f ([]:xss)
>
>   -- prune :: Eq a => [Tree a] -> [[a]] -> ([Tree a], [[a]])
>   prune []               xss = ([], xss)
>   prune (Node v cs : ts) xss
>     = let x = val v
>       in if x `elem` head xss
>            then prune ts xss
>            else let (cs', xss1) = prune cs (tail xss)
>                     (ts', xss2) = prune ts ((x:head xss):xss1)
>                 in (Node v cs' : ts', xss2)


Calculating paths:

> pathsTree :: Tree a -> Tree [a]
> pathsTree  = descend []
>  where descend xs (Node x cs) = Node xs' (map (descend xs') cs)
>           where xs' = x:xs


Breadth first search:

> bfs :: Tree t -> [t]
> bfs  = concat . bft

> bft (Node x cs) = [x] : bff cs
> bff             = foldr (combine (++)) [] . map bft

> combine :: (a -> a -> a) -> [a] -> [a] -> [a]
> combine f (x:xs) (y:ys) = f x y : combine f xs ys
> combine f []     ys     = ys
> combine f xs     []     = xs

> solve :: Puzzle -> IO ()
> solve  = layn
>        . reverse
>        . head
>        . filter (solved . head)
>        . concat
>        . bff
>        . map (pathsTree . mapTree fst)
>        . trimDups (\(p,ps) -> ps)
>        . map (trimRel (\(v,ps) (w,qs) -> vehicle v /= vehicle w))
>        . forest

> layn  :: Show a => [a] -> IO ()
> layn   = putStrLn
>        . unlines
>        . map show


A simple main program:

> main  :: IO ()
> main   = solve puzzle2

