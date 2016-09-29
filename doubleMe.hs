doubleMe x = x + x
doubleLessThan10Number x = (if x < 10 then x*2 else x)

--Fibonacci
fibRecurrence first second = first : fibRecurrence second (first + second)
fibonacci = fibRecurrence 0 1

--Prime, very slow!
--integerFrom1To x = [1..x]
getDem x = [m | m <- take x [1..], (mod x m) == 0]
isPrime x = if length(getDem x) == 2 then 1 else 0

--Prime, hopefully faster
--oddNumberFrom1To x = [1,3..x]
getOddDem x = [m | m <- take x [1,3..], (mod x m) == 0]
isPrimeFast 1 = 1
isPrimeFast 2 = 1 --All even numbers are not prime except for 2
isPrimeFast x = if even x then 0 else if length(getOddDem x) == 2 then 1 else 0

--Prime number list
primeList x = if isPrime x == 1 then x : primeList (x+1) else primeList (x+1)
primeListStartAt1 = primeList 1
primeListFast x = if isPrimeFast x == 1 then x : primeListFast (x+1) else primeListFast (x+1)

--Prime even faster!?
--Numbers that ends with '5' are not prime
--Sums of numbers divisible by 3 are not prime

--Sum of all digit of x
--x mod 10 + x/10 mod 10 + x/100 mod 10...

sumOfAllDigit 0 = 0
sumOfAllDigit x = (mod x 10) + (sumOfAllDigit (div x 10))
-- sumOfAllDigit x = if (x/10) < 1 then x else ((mod x 10) + sumOfAllDigit (floor(x/10)))
-- Does 'if' really necessary here? NOO!

--C equivalent
--int sumOfAllDigit(int x){
--	if (x / 10 < 1) return x;
--	else return x%10 + sumOfAllDigit(x/10);
--}

--Do this in Haskell!?
--int myfnc(int x){
--	if (x < 1) return x - 1;
--	else if (1 <= x <= 5) return 0;
--	else return x + 1;
--}

myFnc x
	| x < 1 = x - 1
	| x <- [1..5] = 0
	otherwise = x + 1

factorial x
	| x > 1 = x * factorial (x - 1)
	| otherwise = 1

--Quicksort
quickSort :: Ord a => [a] -> [a]
quickSort []     = []
quickSort (x:xs) = quickSort [a | a <- xs, a < x] ++ -- Sort the left part of the list
                   [x] ++                            -- Insert pivot between two sorted parts
                   quickSort [b | b <- xs, b >= x]   -- Sort the right part of the list

--try unambiguous re-definition of function
--referential transparency
--Haskell in concurrent world!, in a concurrent world, imperative is the wrong default!, sacrifice memory, state?
--numeric computation in game engine <-- essentially functional

--GUARD
fizzBuzz fb
		| divisibleBy5 && divisibleBy3 = "FizzBuzz"
		| divisibleBy5 = "Fizz"
		| divisibleBy3 = "Buzz"
		| otherwise = show fb

--WHERE both constant and function can be defined in where clause.
--[Expressions where definitions]
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

--LET
--[Let definitions in expressions]
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea

[let square x = x * x in (square 4, square 5, square 6)]
4 * (let a = 9 in a - 2) + 2
[let a = 2; b = 3; c = 4; square x = x*x, square a + square b + square c]

--List comprehension
--[Element(s) | predicate 1, predicate 2..]

--CASE
--case expression of patterns -> results
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."

-- Should use this syntax to avoid Haskell platform giving weird parse error
aaa x = case x of {
             1 ->  "A";
             2 ->  "B";
             3 ->  "C";
			 7 ->  "C";
			 }

-- Making of Types and Typeclasses
data Circle = Circle Float Float Float

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x1 - x2) * abs (y1 - y2)

map (Circle 10 20) [4,5,6,6]
[Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

Car {company = "Ford", model = "Mustang", year = 1967}

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Bool = False | True -- (constructor that take no argument)
data Enum = One | Two | Three
data MaybeInt = NoInt | JustInt Int -- (value constructor)
data Maybe a = Nothing | Just a -- parameterised type
--Maybe isn't a type, it's a type constructor, Maybe Int is a concrete type

--Constructor takes TYPES as arguments!

--Records
data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

--With this definition any type can be assigned to data as long as it is derive 'show'
data Car a b c = Car { company :: a
                     , model :: b
                     , year :: c
                     } deriving (Show)

--Typeclasses
--compare' :: Eq a :: a -> a -> Bool

--Type symnonyms
type String = [Char]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup lockerNumber map =
	case Map.lookup lockerNumber map of
	Nothing -> Left "No locker found"
	Just (state, code) -> if state /= Taken then Right code
						  else Left "Locker is already taken"

--Haskell list
data List a = Empty | List a List a
listHead (List a _) = a
listLast (List a b) | b == [] = a | otherwise listLast b

--Typeclasses
elem _ [] = False
elem x [y:ys] | x == y = True | otherwise = elem x ys

data RGB = RGB Int Int Int
instance Eq RGB where (RGB r1 g1 b1) == (RGB r2 g2 b2) = (r1 == r2) && (g1 == g2) && (b1 == b2)

instance Show RGB where RGB r g b = "Red = " ++ show r ++ " Green = " ++ show g ++ " Blue = " ++ show b
--Output of show should be syntatically correct Haskell statement

data Maybe' a = Nothing' | Just' a
instance (Eq a) => Eq (Maybe' a) where -- a must be of type Eq because == is used to compare Maybe' a later
Nothing' == Nothing' = True
Nothing' == (Just' _) = False
(Just' _) == Nothing' = False
(Just x) == (Just y) = x == y -- <- this is where you need a to be of Eq type

--Instances with context!
--Eq ==
--Ord < > <= >=
--Show show
--Read read derived instances of read will parse string in the format provided by show

instance (Eq a) => Ord a where
--minimun definition of Ord -> 'compare' or '<='

data 2DPoint = 2DPoint Float Float

class Measurable a where
(distance) :: a -> a -> Double

instance Measurable 2DPoint where
distance = someFunction :: a -> a -> Double

--type can be parameterised if we want to keep it generic
type AssocList k v = [(k,v)] --type constructor that takes in 2 types and return a concrete type, AssocList Int String
getValueForKey :: (Eq k) => k -> AssocList k v -> Maybe v

--polymorphic functions ~ generic function
polymorphicF :: (Eq a) => a -> a -> Bool

--Partial apply parameter to type contructor get a new type
type IntMap v = Map Int v
type IntMap = Map Int
--Type in a module has to be preceeed by module name
type IntMap = Map.Map Int

--Make sure to differentiate between type contructor and value contructor. All you can do to type contructor is refer to its using different name

data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord) -- notice the ordering of a and List a is different than using Cons a (List a)

infixr 5  ++
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
	yesno 0 = False
	yesno _ = True

instance YesNo [a] where
	yesno [] = False
	yesno _ = True

instance YesNo Bool where
    yesno = id     -- id is a standard function that takes in a parameter and return the same thing

class Eq a where  -- a is a type!
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)

--Functor
class Functor f where
    fmap :: (a -> b) -> f a -> f b
	fmap' :: (a -> b -> c) -> f a -> f b -> f c
--fmap takes a function from one type to another and a functor applied with one type and returns a functor applied with another type.
--f in this case has to be a type constructor that take in one type
--Functor typeclasses wants to take in a constructor


instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

(a -> b) -> Maybe a -> Maybe b
(a -> b) -> Maybe m a -> Maybe m b --doesnt make sense

(a -> b) -> Tree a -> Tree b
instance Functor Tree where
	fmap f (Node a lhs rhs) = Node (f a) f lhs f rhs

(a -> b) -> (Either c) a -> (Either c) b
instance Functor (Either a) where
	fmap f (Right x) = Right (f x)
	fmap f (Left x) = Left x
--Can we map the Left?

--Try figuring out how Map k is made an instance of Functor by yourself!
--partially apply type
type MyType = Either Float

class Tofu t where
    tofu :: j a -> t a j

* -> (* -> *) -> *

data tofu' a = a
data tofu a tofu' = a

data Frank a b  = Frank {frankField :: b a} deriving (Show)

class Associative f where
pure :: a -> f a
<*> :: f (a -> b) -> f a -> f b

pure (+) <*> Just 3 <*> Just 5
Just 8

pure (+) -> Just (+)
Just (+) <*> Just 3 -> Just (+3)
Just (+3) <*> Just 5 -> Just 8

--pure f <*> x is equivalent to fmap f x

pure (+) <*> Just 3 <*> Just 5
-- Can be rewritten as
(+) <$> Just 3 <*> Just 5

-- A functor with function and map the function to other functor

[(+1)] <*> [1] -- [] - functor

(+1) -- functor?

[(*0),(+100),(^2)] <*> [1,2,3]
[0,0,0,101,102,103,1,4,9]

:t (+) <$> (+3) <*> (*100)
(+) <$> (+3) <*> (*100) :: (Num a) => a -> a

(+) <$> (+3) <*> (*100) $ 5
508

(+3) <*> (*100)

(+) <$> (+3) -> +(+3)
+(+3) <*> (*100) -> + (+3) (*100)

--f <$> a is equivalent to fmap f a or pure f <*> a

map :: (a -> b) -> [a] -> [b]

pure (+) <*> Just 3 <*> Just 5  -- left associative
-> Just (+) <*> Just 3 <*> Just 5
-> Just (+3) <*> Just 5
-> Just 8
--The function can take as many parameters as we want, because it's always partially applied step by step between occurences of <*>.
