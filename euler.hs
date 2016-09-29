divisibleBy5 x = mod x 5 == 0
divisibleBy3 x = mod x 3 == 0

fizzBuzz fb 
		| divisibleBy5 fb && divisibleBy3 fb = "FizzBuzz"
		| divisibleBy5 fb = "Fizz"
		| divisibleBy3 fb = "Buzz"
		| otherwise = show fb
						
--Euler 1 - Sum of multiples of 3 and 5 list
sumOfMultiplesOf3And5 x = foldl (\x y -> x + y) 0 (filter (\x -> mod x 3 == 0 || mod x 5 == 0) x)

--Euler 2 - Even Fibonacci (Side note: what is Fibonacci use for?)
fib 1 = 0
fib 2 = 1
fib x = fib (x - 1) + fib (x - 2) --This is the definition of fibonacci, but it is rather slow as it has to recontruct all previous values in sequence

fibList x = fib x : fibList (x + 1) 
fibList' x = fib x : fibList (x - 1) -- Still slow!

fibFast first second = first : fibFast second (first + second)
fibListFast = fibFast 0 1
--When constructing build up list, it is better to build the list from left to right, as it working out the list as you go!

evenFib = filter (\x -> mod x 2 == 0) fibListFast

--Type
data Circle = Circle Float Float Float
--Euler 3
--Largest prime factor

--isPrime 
--What's the fastest way to find a prime number?
--Try division to all number that it less than it
--Try division to all prime number less than it.
--1,2,3,5,7,11

head' xs = case xs of [] -> error "No head for empty lists!"  
                      xs ->  "No head for empty lists!" 
					  

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  
--getPrime
isDivisible :: Integral a => a -> [a] -> Bool
isDivisible x [y] = mod x y == 0
isDivisible x (xs:xy) = case xs of { 0 -> error "Divide by 0";
									   x -> mod x xs == 0 || isDivisible x xy
									  }

isNotDivisible :: Integral a => a -> [a] -> Bool
isNotDivisible x [y] = mod x y /= 0
isNotDivisible x (y:ys) = (mod x y) /= 0 && isNotDivisible x ys								  

narrowList :: Integral a => a -> [a] -> [a]
narrowList y x = [z | z <- x, z <= div y 2]

beginPrime :: Integral a => a -> [a] -> [a]
beginPrime y x = if isNotDivisible y x then y : beginPrime (y+1) (y:x) 
									else beginPrime (y+1) x 
primes :: Integral a => [a]
primes = 1:2: beginPrime 3 [2]

largestPrimeFactor x = last [y | y <- take x primes, y < x, mod x y == 0]
allPrimeFactors x = [y | y <- take x primes, y < x, mod x y == 0]

--Euler 4
isPalindrome :: [Char] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome [x,y] = x == y
isPalindrome x = head x == last x && isPalindrome (init (tail x))

--list implementation
palindromes :: Int -> Int -> [Int]
palindromes bound1 bound2 = [x * y | x <- [1..bound1], y <- [1..bound2], isPalindrome (show (x * y))]

--compare' :: Eq a :: a -> a -> Bool

--palindromes' bound1 bound2 = if isPalindrome (show (bound1 * bound2)) then [(bound1, bound2, bound1*bound2)] ++ palindromes bound1 bound2-1 ++ palindromes bound1-1 bound2
--999 999, 999 998, 998 999, 999 997, 998 997 <- very slow

--palindromes'' (hx:x) (hy:y) = if isPalindrome (show (hx * hy)) then

--palindromes'' :: Num a => a -> [a] -> [(a,a,a)]
--palindromes'' a b = if isPalindrome (show (a * b)) then [(a, b, a * b)] else [(0,0,0)]
--palindromes'' a (h:b) = if isPalindrome (show (a * h)) then (a, h, a * h) : palindromes'' a b else palindromes'' a b

--palindromes''' (h:a) b = palindromes'' h b : palindromes''' a b

--Thoughts so far
--Thinking function, especially in terms of Haskell is quite different. The thing that concern me the most is the efficiency of the code
--Haskell is granted very powerful in its syntax, you can archive great deal with very concise definition
--However, it is difficult to know the efficiency of your code
--Solution or the way(how) to solve a problem can be change quite easily. I can see if a solution is efficient or not at this level, but it will get complicated later on.

