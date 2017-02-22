This is a worksheet with some exercises about "tying the knot",
a programming technique that exploits lazy evaluation in some
interesting ways ...

> module KnotWorksheet where

> import Data.List(transpose)

-------------------------------------------------------------------
Question 1:  Here is a simple tree datatype:

> data LTree a = Leaf a
>              | Fork (LTree a) (LTree a)
>                deriving (Show)

and here are some values of that type:

> ex1, ex2 :: LTree Int
> ex1       = Fork (Fork (Leaf 2) (Leaf 3)) (Fork (Leaf 4) (Leaf 1))
> ex2       = foldr1 Fork (map Leaf [1,4,7,10,2,8,3,6,9])

Write a function:

> repMax  :: Ord a => LTree a -> LTree a
> repMax   = undefined

that maps the input tree to an output tree of the same shape in
which every Leaf node has been replaced by the maximum value in
the input tree, and which makes only a single traversal over the
input tree structure.

You'll probably find it useful to do this using an auxiliary
function as follows:

> repMax' :: Ord a => a -> LTree a -> (a, LTree a)
> repMax'  = undefined

Here are some examples to illustrate the expected behavior:

  KnotWorksheet> repMax ex1
  Fork (Fork (Leaf 4) (Leaf 4)) (Fork (Leaf 4) (Leaf 4))
  KnotWorksheet> repMax ex2
  Fork (Leaf 10) (Fork (Leaf 10) (Fork (Leaf 10) (Fork (Leaf 10)
  (Fork (Leaf 10) (Fork (Leaf 10) (Fork (Leaf 10) (Fork (Leaf 10)
  (Leaf 10))))))))
  KnotWorksheet> 

-------------------------------------------------------------------
Question 2:  For the same LTree datatype described in Question 1,
write a function that maps a tree of integers to a tree of doubles.
The sum of the double values should be one, shared between the leaf
nodes of the tree in proportion to the integer weights in the input.
For example:

  KnotWorksheet> share ex1
  Fork (Fork (Leaf 0.2) (Leaf 0.3)) (Fork (Leaf 0.4) (Leaf 0.1))
  KnotWorksheet> share ex2
  Fork (Leaf 0.02) (Fork (Leaf 0.08) (Fork (Leaf 0.14) (Fork (Leaf 0.2)
  (Fork (Leaf 0.04) (Fork (Leaf 0.16) (Fork (Leaf 0.06) (Fork (Leaf 0.12)
  (Leaf 0.18))))))))
  KnotWorksheet>

Of course, the function that you define should only make a single
pass over the input tree structure.  The following should provide
a suitable starting point:

> share   :: LTree Int -> LTree Double
> share    = undefined

> share'  :: Double -> LTree Int -> (Double, LTree Double)
> share'   = undefined

-------------------------------------------------------------------
Question 3: Let's suppose that we represent a table of information
as a list of lists of strings:

> table :: [[String]]
> table  = [ [ "Banana",    "Yellow", "Potassium" ],
>            [ "Apple",     "Green",  "Fiber"     ],
>            [ "Orange",    "Orange", "Vitamin C" ],
>            [ "Burger",    "Brown",  ""          ],
>            [ "Blueberry", "Purple", "Antioxidants" ] ]

Its not too hard to print out a table in a prettier format if we
specify the list of column widths that we'd like as a parameter:

> showTable     :: [Int] -> [[String]] -> IO ()
> showTable cols = putStr . unlines . separate cols . map (fmt cols)

Here, the fmt function handles the task of formating a single row,
adding padding and vertical bars between columns:

> fmt          :: [Int] -> [String] -> String
> fmt cols vals = foldr (\xs ys -> "|" ++ xs ++ ys) "|"
>               $ zipWith pad cols vals
>                 where pad n s = take n (s ++ repeat ' ')

And the separate function joins together multiple rows to form a
table by adding lines of + and - characters:

> separate     :: [Int] -> [String] -> [String]
> separate cols = foldr (\r t -> sepRow : r : t) [sepRow]
>  where sepRow = foldr (\c r -> "+" ++ replicate c '-' ++ r) "+" cols

Now we can get output like the following:

  KnotWorksheet> showTable [3,4,5] table
  +---+----+-----+
  |Ban|Yell|Potas|
  +---+----+-----+
  |App|Gree|Fiber|
  +---+----+-----+
  |Ora|Oran|Vitam|
  +---+----+-----+
  |Bur|Brow|     |
  +---+----+-----+
  |Blu|Purp|Antio|
  +---+----+-----+

  KnotWorksheet> showTable [12,12,12] table
  +------------+------------+------------+
  |Banana      |Yellow      |Potassium   |
  +------------+------------+------------+
  |Apple       |Green       |Fiber       |
  +------------+------------+------------+
  |Orange      |Orange      |Vitamin C   |
  +------------+------------+------------+
  |Burger      |Brown       |            |
  +------------+------------+------------+
  |Blueberry   |Purple      |Antioxidants|
  +------------+------------+------------+

  KnotWorksheet> 

Wouldn't it be nice if we could just show a table directly without
having to specify explicit column widths?  Surely those column
widths could be calculated for us explicitly?  Wouldn't it be nice
if this could be done in only one traversal of the data structure?

That's your task in this question: write a function

> showTable'  :: [[String]] -> IO ()
> showTable'   = undefined

that will print out an arbitrary table, automatically sizing the
columns to fit the longest string in each one.  For example:

  KnotWorksheet> showTable' table
  +---------+------+------------+
  |Banana   |Yellow|Potassium   |
  +---------+------+------------+
  |Apple    |Green |Fiber       |
  +---------+------+------------+
  |Orange   |Orange|Vitamin C   |
  +---------+------+------------+
  |Burger   |Brown |            |
  +---------+------+------------+
  |Blueberry|Purple|Antioxidants|
  +---------+------+------------+

  KnotWorksheet>

Of course, you should reuse as much of the previous code as
possible (fmt and separate, for example).  The following would
probably be a good starting point :-)

  showTable' rows = putStr $ unlines $ separate cols $ map snd ps
   where ps          :: [([Int], String)]
         ps           = map analyze rows
         
         cols        :: [Int]
         cols         = ...
         
         analyze     :: [String] -> ([Int], String)
         analyze vals = ...

Note that I imported the transpose function from Data.List at the
top of this worksheet; you might find it helpful to use that
function in your calculation of cols.  If you don't know what
transpose does, give it a try, ask for help, or ponder the following
example:

  Main> showTable' (transpose table)
  +---------+-----+---------+------+------------+
  |Banana   |Apple|Orange   |Burger|Blueberry   |
  +---------+-----+---------+------+------------+
  |Yellow   |Green|Orange   |Brown |Purple      |
  +---------+-----+---------+------+------------+
  |Potassium|Fiber|Vitamin C|      |Antioxidants|
  +---------+-----+---------+------+------------+

  Main>

-------------------------------------------------------------------
