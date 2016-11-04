module DefiningTrees where

-- The following list describes a number of
-- different kinds of tree data structure:
--
-- 1 - Binary trees in which each interior
--     node hold a single Int value, and
--     leaf nodes are empty.
--
-- 2 - Binary trees in which each leaf node
--     holds a single Int value and interior
--     nodes do not carry any data.
--
-- 3 - Binary trees in which each leaf node
--     holds a Boolean and each interior node
--     holds an Int.
--
-- 4 - Binary trees in which each interior
--     node has an associated Int value and
--     each edge has an associated Char value.
--
-- For each of these different tree types:
-- a) construct a definition for that kind of
--    tree using an algebraic datatype definition.
-- Replace Undefinedn with some real constructors.

data Tree1 = Undefined1
data Tree2 = Undefined2
data Tree3 = Undefined3
data Tree4 = Undefined4

-- b) construct a list of length (at least) four
--    that contains distinct elements of your new
--    tree type.

examples1 :: [Tree1]
examples1  = [ undefined ]

examples2 :: [Tree2]
examples2  = [ undefined ]

examples3 :: [Tree3]
examples3  = [ undefined ]

examples4 :: [Tree4]
examples4  = [ undefined ]

-- c) define functions that can compute the depth of
--    an arbitrary tree, one function for each of these
--    types:

depth1 :: Tree1 -> Int
depth1  = undefined

depth2 :: Tree2 -> Int
depth2  = undefined

depth3 :: Tree3 -> Int
depth3  = undefined

depth4 :: Tree4 -> Int
depth4  = undefined

-- Done! ------------------------------------
