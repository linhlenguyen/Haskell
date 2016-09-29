factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x-1)

sumFactorial :: Integer -> Integer
sumFactorial n = if n < 10 then factorial n
						   else factorial $ mod n 10 + sumFactorial (div n 10)

sumDigit :: Integer -> Integer
sumDigit n = if n < 10 	then n 
						else mod n 10 + sumDigit (div n 10)

nearestFactorial' :: Integer -> Integer -> Integer
nearestFactorial' _ 1 = 0
nearestFactorial' i n = if factorial (i+1) > n then i else nearestFactorial' (i+1) n

nearestFactorial :: Integer -> Integer
nearestFactorial n = nearestFactorial' 0 n
						
findSmallestN :: Integer -> Integer
findSmallestN n = let nf = nearestFactorial n 
				  in case nf of { 0 -> 1;
								  1 -> 1;
								  x -> if x == n then x else x + (findSmallestN (n - (factorial x))) * 10;
								}
					
--reduceN :: Integer -> Integer
--reduceN = 
								
numbersWithSumOfDigitsEqual :: Integer -> [Integer]
numbersWithSumOfDigitsEqual n = [x | x <- [1..], sumDigit x == n] --What's the upper bound?

findG :: Integer -> [Integer]
findG n = map findSmallestN $ (numbersWithSumOfDigitsEqual n)

