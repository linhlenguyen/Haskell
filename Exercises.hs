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
type NormalizedQuery = Char
type ProductId = Int

stq :: [(SessionId, NormalizedQuery)]
stq = [(1, 'a'), (1, 'b'), (2, 'a'), (3, 'c')]

stp :: [(SessionId, ProductId)]
stp = [(1, 101), (1, 103), (1, 104), (2, 102)]

qts :: (Ord b) => [(a, b)] -> Map b [a]
qts xs = Data.Foldable.foldl' foldf Data.Map.empty xs
  where foldf :: (Ord b) => Map b [a] -> (a, b) -> Map b [a]
        foldf m (a, b) = case (Data.Map.lookup b m) of
                                { Nothing -> insert b [a] m;
                                  Just x -> insert b (a:x) m ; }

stlp :: (Ord a) => [(a, c)] -> Map a [c]
stlp xs = Data.Foldable.foldl' foldf Data.Map.empty xs
  where foldf :: (Ord a) => Map a [c] -> (a, c) -> Map a [c]
        foldf m (a, c) = case (Data.Map.lookup a m) of
                                { Nothing -> insert a [c] m;
                                  Just x -> insert a (c:x) m; }

qtp qts stp = Data.Map.map mf qts
  where mf as = concatMap (stp!) as



{-|
4.) You're given an array which contains the numbers from 1 to n, in
random order, except there is one missing. Write a function to return
the missing number.
-}

--sort then search O(nlogn)
--traverse then find O(n)
--sum n and subtract O(n)
--


{-|
5.) Write a function to reconstruct a binary tree from its preorder
traversal and inorder traversal. Take into account that the traversals
could be invalid.
-}

data Tree a = Empty | Cons a (Tree a) (Tree a) deriving (Show)

addNode :: (Ord a) => a -> Tree a -> Tree a
addNode a Empty = (Cons a Empty Empty)
addNode a (Cons v l r) = if (v <= a) then (Cons v (addNode a l) r) else (Cons v l (addNode a r))

reconstructTree :: (Ord a) => [a] -> Tree a
reconstructTree xs = Prelude.foldr (addNode) Empty xs


{-|
6.) You have a [(WeatherStationId, Latitude, Longitude)]. Similar to
#3, write a function which will, off-line, turn this into a data
structure from which you can easily determine the nearest Weather
Station, given an arbitrary Latitude and Longitude.
-}

{-
 1 2 3 4 5
1
2  a
3        c
4    b
5        d
-}

--(1,3) -> a
--[(x,y)]

type WeatherStationId = Int
type Latitude = Float
type Longitude = Float

weatherStations :: [(WeatherStationId, Latitude, Longitude)]
weatherStations = zip3 [1, 2, 3, 4, 5, 6] [12.4,21.6,23.7,44.0,100.0,200] [9.0, 98.0, 45.2, 2.0, 80.5, 121.5]

findNearestWeatherStation :: Latitude -> Longitude -> WeatherStationId
findNearestWeatherStation = undefined

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

shuffle :: [a] -> [a]
shuffle a = undefined

type Coin = Int

coins :: [Coin]
coins = [1,2,5,10,20,50,100,200]

--56
--56x1
--54x1 1x2

--1 ~ 1 *
--2 ~ 2x1, 2 *
--3 ~ 3x1, 1 + 2
--4 ~ 4x1, 2x1 + 2, 2x2
--5 ~ 5x1, 3x1 + 2, 2x2 + 1, 5 *
--6 ~ 6x1, 4x1 + 2, 2x1 + 2x2, 3x2, 5 + 1
--7 ~ 7x1, 5x1 + 2, 3x1 + 2x2, 1 + 3x2, 2x1 + 5, 2 + 5
--8 ~ 8x1, 6x1 + 2, 4x1 + 2x2, 2x1 + 3x2, 4x2, 1x3 + 5, 1 + 2 + 5,
--9 ~ 9x1, 7x1 + 2, 5x1 + 2x2, 3x1 + 3x2, 1 + 4x2, 4x1 + 5, 2x1 + 2 + 5, 2x2 + 5
--10 *
--21 ~ 21x1, 19x1 + 2 .. 1 + 10x2, 16x1 + 5 .. 1 + 8x2 + 5, 11x1 + 2x5 .. 1 + 5x2 + 2x5, 11x1 + 10 .. 1 + 5x2 + 10, 6x1 + 5 + 10, 3x2 + 5 + 10, 1 + 2x5 + 10, 1 + 2x10

findP :: Int -> [Coin] -> [[(Int, Coin)]]
findP _ [] = []
findP 0 _ = []
findP s (x:xs) = let pn = [(div s x)..0] in
  case pn of {  [0] -> findP s xs;
                _ -> map (\d -> (d,x) : (findP (s - (d*x)) (x:xs))) pn;
              }

-- dp ~
-- i <- coins
-- j <- coins[i], j <= amount
-- dp[j] += dp[j - coins[i]]

findP' :: Int -> [Coin] -> [(Int, Coin)]
findP' s (x:xs) = f s (reverse (x:xs))
 where f :: Int -> [Coin] -> [(Int, Coin)]
       f _ [] = []
       f s (x:xs) = let d = div s x
                        r = mod s x in
                      if d > 0 then (d,x):(f r xs)
                      else (f s xs)

                      -- first 15 mins 1-1000
                      -- +4
                      -- p1 - 1 -> 375
                      -- p2 - 125 -> 500
                      -- p3 - 500 -> 875
                      -- p4 - 625 -> 1000
                      -- +2
                      -- next 15 mins 125-375
                      -- p1
                      -- p2
                      -- p3
                      -- p4

                      -- 4 -> 16 -> 64 -> 256
                      -- 5 -> 25 -> 125 -> 625
                      -- 6 -> 36 -> 216 -> 1296
