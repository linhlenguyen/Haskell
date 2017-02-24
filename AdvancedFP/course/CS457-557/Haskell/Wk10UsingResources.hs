module UsingResources where

-- (1) Find out how to use multiline string constants
-- so one doesn't need to use the append (++)
-- operator in the code fragment below.


address = "Tom Smith\n" ++
          "100 Main Street\n" ++
          "Everytown, OR 4275355"


-- (2) Use "as-patterns" to rewrite the insert function
-- below so the pattern guard, guarded by (n==m), doesn't
-- have to reconstruct the input (Fork l m r) to insert.

data Tree = Leaf | Fork Tree Int Tree

insert :: Int -> Tree -> Tree
insert n Leaf = Fork Leaf n Leaf
insert n (Fork l m r)
   | n < m = Fork (insert n l) m r
   | n > m = Fork l m (insert n r)
   | n==m  = Fork l m r


-- (3) Add an infix declaration to the program so that sumMod3
-- can be used as an infix operator with left associativity
-- and the same precedence as (+). Thus
-- x `sumMod3` y * 3 + 4
--      would parse as
-- (sumMod3 x (y*3)) + 4

sumMod3 x y = mod (x++y) 3



-- (4) The stripPrefix function belongs to one of the standard
-- libraries. Find it, put an import declaration in your
-- file to import it. What is its type? Use it in an example.

-- example = .... stripPrefix


-- (5) Rewrite the function myEven using n+k patterns
-- This only works in older version of GHC. It has been
-- removed in newer versions!

myEven 0 = True
myEven 1 = False
myEven n = myEven (n-2)
