factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x-1)

(!) :: Integer -> Integer -> Integer
(!) n m = div (factorial n) (factorial m * (factorial (n-m)))

pf :: Integer -> Double
pf n = 1/(2^n)

valueAt :: Integer -> Double -> Integer -> Integer -> Double
valueAt v k n m = (n ! m) * v * (1 - k) ^ n * (1 + k)^m



