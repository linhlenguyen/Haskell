import Data.Map
import Data.Foldable

{-|
type families and dependent types

easy: generate a list of primes
medium: write a function that gives the nth Fibonacci number
hard: demonstrate the use of a Monad

advantages and disadvantages of Monad, Applicative and Arrow

Write a function to sort a list by frequencies of the elements. The element that appears in the list least frequently should go first and the element that is the most frequent should go last. For example, [3,2,1,2] should be ordered as [3,1,2,2].
-}

{-|
1.) A "rotated array" is an array of integers in ascending order,
after which for every element i, it has been moved to element (i + n)
mod sizeOfList. Write a function that takes a rotated array and, in
less-than-linear time, returns n (the amount of rotation).
-}
--[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
--n = 3, size = 10
--0 -> 3, 1 -> 4, 2 -> 5..
--n = 9, size = 10
--0 -> 9, 1 -> 0

shiftN :: Int -> [a] -> [a]
shiftN n xs = (drop ln xs) ++ (take ln xs)
  where ln = (length xs) - n

findN :: (Ord a) => [a] -> Int
findN xs = findN' 0 xs

findN' :: (Ord a) => Int -> [a] -> Int
findN' i [x1,x2] = i+1
findN' i (x1:x2:xs) = if (x2 < x1) then i+1 else findN' (i+1) (x2:xs)

{-|
2.) You are given a list of Ball objects. Each Ball is either Red or
Blue. Write a function that partitions these balls so that all of the
balls of each color are contiguous. Return the index of the first ball
of the second color (your result can be Red balls, then Blue balls, or
the other way around). In haskell, you'll probably want to return a
([Ball],Int).
-}

data Ball = Red | Blue deriving (Show, Eq)

sortBall :: [Ball] -> ([Ball], Int)
sortBall bs = (reds ++ blues, i)
  where split :: ([Ball], [Ball]) -> [Ball] -> ([Ball], [Ball])
        split a [] = a
        split (reds, blues) (b:bs) = if b == Red then split (b:reds, blues) bs else split (reds, b:blues) bs
        (reds, blues) = split ([],[]) bs
        i = length reds

{-|
3.) Live Search is a search engine. Suppose it was to be tied into an
online store. Now you're given two lists. One is a [(SessionId,
NormalizedQuery)]. That is, when a particular user performs a query,
it is turned into some consistent format, based on their apparent
intent, and stored in this logfile. The second list is a [(SessionId,
ProductId)]. This indicates the product bought by a particular user.
Now, you want to take these two (potentially very long) lists, and
return some structure that will make it easy to take a query and
return a list of the most popular resulting purchases. That is, of
people who have run this query in the past, what are the most common
products they've wound up buying? The interviewer said that this is an
instance of a well-known problem, but I didn't catch the name of it.
-}

type SessionId = Int
type NormalizedQuery = String
type ProductId = Int

{-|
SessionId -> Query
1 -> 'a'
1 -> 'b'
2 -> 'a'
3 -> 'c'

Product
1 -> p1
1 -> p3
1 -> p4
2 -> p2

Query -> [SessionId] -> [Product]
[(Query, [Product])]
-}

queryToSessionID :: (Ord b) => [(a, b)] -> Map b [a]
queryToSessionID xs = Data.Foldable.foldl' foldf Data.Map.empty xs
  where foldf :: (Ord b) => Map b [a] -> (a, b) -> Map b [a]
        foldf m (a, b) = case (Data.Map.lookup b m) of
                                { Nothing -> m;
                                  Just x -> insert b (a:x) m ; }

normaliseSessionProduct :: (Ord a) => [(a, c)] -> Map a [c]
normaliseSessionProduct xs = Data.Foldable.foldl' foldf Data.Map.empty xs
  where foldf :: (Ord a) => Map a [c] -> (a, c) -> Map a [c]
        foldf m (a, c) = case (Data.Map.lookup a m) of
                                { Nothing -> m;
                                  Just x -> insert a (c:x) m; }

queryToProduct :: Map b [a] -> [(a, c)] -> Map b [c]
queryToProduct m (x:xs) = undefined

{-|
4.) You're given an array which contains the numbers from 1 to n, in
random order, except there is one missing. Write a function to return
the missing number.
-}

{-|
5.) Write a function to reconstruct a binary tree from its preorder
traversal and inorder traversal. Take into account that the traversals
could be invalid.
-}

{-|
6.) You have a [(WeatherStationId, Latitude, Longitude)]. Similar to
#3, write a function which will, off-line, turn this into a data
structure from which you can easily determine the nearest Weather
Station, given an arbitrary Latitude and Longitude.
-}

{-|
7.) Write a function for scoring a mastermind guess. That is, in a
game of mastermind
(http://en.wikipedia.org/wiki/Mastermind_(board_game)), given a guess,
and the actual answer, determine the number correct, the number wrong,
and the number out of place.
-}

{-|
8.) Implement a trie (http://en.wikipedia.org/wiki/Trie) data
structure. Write a function add, which takes a word, and a trie, and
adds the word to the trie. Write another function lookup, which takes
a prefix and a trie, and returns all the words in the trie that start
with that prefix.
-}

{-|
9.) Write an algorithm to shuffle a deck of cards. Now write a
function to perform some kind of evaluation of "how shuffled" a deck
of cards is.
-}
