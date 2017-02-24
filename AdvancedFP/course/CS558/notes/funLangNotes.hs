-- First class functions

-- Anonymous functions
h1 x = x + 4
h2 = \ x -> x + 4

-- nested functions

map1 (g,u) =
  let f [] = []
      f (x:xs) = (g x) : (f xs)
  in f u
  
{-
This acts similarly to other nested declarations:

Parameters and local variables of outer functions are 
visible within inner functions (using lexical scoping rules).

Purpose: localize scope of nested functions, and avoid the 
need to pass auxiliary parameters defined in outer scopes.

Semantics of a function definition now depend on values of
function’s free variables .  
-}

-- FUNCTIONS AS RETURN VALUES
map2 g =
  let f [] = []
      f (x:xs) = (g x) : (f xs)
  in f
  

{- 
In fact, we can simplify still further by rewriting map to
return the nested function "f"

Now we need a slightly different calling convention, passing
the two arguments separately:
-}
inc x = x+1

w1 = ((map2 inc) [1,2,3])

-- yields [2;3;4]

-- But now we can also choose to pass just one argument at a time

minc = map2 inc
w2 = minc [1,2,3]
w3 = minc [4,5,6]

-- Curried functions take multiple arguments
-- but we can apply the function to fewer arguments

map3 g u  =
  let f [] = []
      f (x:xs) = (g x): (f xs)
  in f u

w4 = map3 inc [2,5,7]

-- function application associates to the left, so

w5 = (map3 inc) [4,7,8]

{- Notice difference in types  
map1 :: (t -> a, [t]) -> [a]

map3 :: (t -> a) -> [t] -> [a]

The function aroow associates to the right so

map3:: (t -> a) -> ([t] -> [a])

Notice curried functions return functions!
-}


-- Currying is most often useful when passing partially
-- applied functions to other higher-order functions:

pow n b = if n==0 then 1 else b * (pow (n-1) b)

w6 = map (pow 3) [1,2,3]
-- evaluates to [1,8,27]

-- we can store functions in data structures
-- we can create a list of functions

w7 = map pow [0,1,2,3,4]
[f0,f1,f2,f3,f4] = w7
-- try applying f0,f1 etc. to 2, or 5

-- Anonymous functions

w8 = map (\ x -> x+5) [2,5,7]

{-
Name derives from the Alonzo Church’s “lambda calculus,”
or iginally invented to study computability theory, in
which anonymous functions would be written using the
Greek lambda character.
-}

-- Lets capture another pattern (other than map)

-- Add a list of integers
sum2 [] = 0
sum2 (x:xs) = x + sum2 xs

-- multiply a list of integers
prod2 [] = 1
prod2 (x:xs) = x * prod2 xs

w9 = sum2 [3,4,6]
w10 = prod2 [2,6,3]

-- Here are a few more

copy [] =[]
copy (x:xs) = x : copy xs

len [] = 0
len (x:xs) = 1 + len xs

-- This function is called foldr in Haskell
-- I called it fold, so that it doesn't conflict.
fold f n [] = n
fold f n (x:xs) = f x (fold f n xs)

sum3 xs  = fold (\ x y -> x+y) 0 xs
prod3 xs = fold (\ x y -> x*y) 1 xs
copy3 xs = fold (\ x y -> x : y) [] xs
len3 xs  = fold (\ x y -> 1 + y) 0 xs
elem3 x xs = fold (\ item ans -> item==x || ans) False xs

{-
Function foldr computes a value working from the tail of
the list to the head (from right to left). Argument n is
the value to return for the empty list. Argument f is the
(Curried) function to apply to each element and the
previously computed result.

foldr (+) 8 (x1 : x2 : x3 : x4 : [])

 x1 + x2 + x3 + x4 + 8
 
 f x1 (f x2 (f x3 (f x4 8)))
 -} 
 
-- Accumulating functions
-- Recall
-- len [] = 1
-- len (x:xs) = 1 + len xs
-- Note that after the recursive call, we still need to add 1.

len4 [] n = n
len4 (x:xs) n = len4 xs (1+n)

len5 xs = len4 xs 0 -- note we need to intialize the acc

prod4 [] n = n
prod4 (x:xs) n = prod4 xs (x*n)

prod5 xs = prod4 xs 1 -- here intitialization is 1

-- We might do this because these functions are
-- tail recursive and can be efficiently compiled!

-- Another way to write tail recursive functions
-- is to use continuation style. Every function gets
-- an extra argument

len6 :: [a] -> (Int -> b) -> b
len6 [] k = k 0
len6 (x:xs) k = len6 xs (\ n -> k (n+1))

w12 = len6 [2,5,7] id

prod6 [] k = k 1
prod6 (x:xs) k = prod6 xs (\ n -> k (x*n))

{-
Notice that every call is now a tail-call . Note too that
len6 only returns after the outer k is invoked; in essence,
it needn’t return at all. If it were the whole program,
it wouldn’t need to return at all. This is because k is
serving the same role as a return address: saying “what
to do n ext.”

This means we can evaluate len6 without a stack !
Functions like k are called continuations and programs
written using them are said to be in continuation-passing
style (CPS)


We may choose to write (parts or all of) programs
explicitly in CPS because it makes it easy to express a
particular algorithm or because it clarifies the control
structure of the program.


Note that CPS programs are just a subset of ordinary
function al programs that happens to make heavy use of
the (existing) enormous power of first-class functions.
Remarkably, we can also systematica lly convert any
functional program into an equivalent CPS program.
(Details omitted.)
-}
 
 