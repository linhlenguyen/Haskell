module BaseConversion()
where
  --Check if the value of a binary number
  --(passed as a string) equals the hexadecimal representation of a string.

  toInt :: Int -> String -> Int
  toInt base string = toInt' base 0 string

  toInt' :: Int -> Int ->> String -> Int
  toInt' _ _ [] = []
  toInt' base power (x:xs) = x*base^power + toInt' xs
