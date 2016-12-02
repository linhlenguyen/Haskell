module Arithmetic()
where
  --Addition without any arithmetic operator

  --1011 1011 = 187
  --0011 1111 = 63

  --1000 0100 XOR
  --0011 1011 AND
  --0111 0110 <<
  --1111 0010 XOR
  --0000 0100 AND
  --0000 1000 <<
  --1111 1010 XOR
  --0000 0000 AND

  --1111 1010 = 250

  binaryToInt :: [Char] -> Int
  binaryToInt ls = stringToInt' 0 2 $ reverse ls

  stringToInt' :: Int -> Int -> [Char] -> Int
  stringToInt' _ _ [] = 0
  stringToInt' power base (x:xs) = (charToInt x)*(base^power) + (stringToInt' (power+1) base xs)

  charToInt :: Char -> Int
  charToInt c = case c of { '0' -> 0;
                            '1' -> 1;
                            '2' -> 2;
                            '3' -> 3;
                            '4' -> 4;
                            '5' -> 5;
                            '6' -> 6;
                            '7' -> 7;
                            '8' -> 8;
                            '9' -> 9;
                            'A' -> 10;
                            'B' -> 11;
                            'C' -> 12;
                            'D' -> 13;
                            'E' -> 14;
                            'F' -> 15; }
