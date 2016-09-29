newtype Writer w a = Writer { writer :: (a, w) }

instance (Monoid w) => Monad (Writer w) where
return a = Writer (a, mempty)
-- m a -> (a -> m b) -> m b
-- Writer w a -> (a -> Writer w b) -> Writer w b
(Writer (a,w)) >>= f = let (b,w') = f a
					   in Writer (b, w 'mappend' w')

zip' :: (Maybe a -> Maybe a -> b) -> [a] -> [a] -> [b]
zip' _ [] [] = []
zip' f [] (y:ys) = f Nothing (Just y) : zip' f [] ys
zip' f (x:xs) [] = f (Just x) Nothing : zip' f xs []
zip' f (x:xs) (y:ys) = f (Just x) (Just y) : zip' f xs ys

consolidatePlus' :: Char -> [(Char, Char)] -> [Char]
consolidatePlus' carry [] = if carry == '1' then [carry] else []
consolidatePlus' carry ((x1,x2):xs) = let (y1,y2) = carry +++ x2 
									in case y1 of {
										'1' -> y2 : consolidatePlus' '1' xs; 
										_ -> y2 : consolidatePlus' x1 xs;
									}

consolidatePlus :: [(Char, Char)] -> [Char]
consolidatePlus = consolidatePlus' '0'

(<+>) :: [Char] -> [Char] -> [Char]
(<+>) x y = reverse $ consolidatePlus $ (reverse x ++++ reverse y)

(<++>) :: [Char] -> [Char] -> [Char]
(<++>) x y = consolidatePlus $ x ++++ y

(++++) :: [Char] -> [Char] -> [(Char,Char)]
(++++) = zip' plusM

plusM' :: Int -> Maybe Char -> Maybe Char -> (Char,Char)
plusM' b Nothing (Just y) = ('0', y)
plusM' b (Just x) Nothing = ('0', x)
plusM' b (Just x) (Just y) = plus' b x y

plus' :: Int -> Char -> Char -> (Char,Char)
plus' b x y = let s = (toInt x) + (toInt y) 
				in if s < b then ('0', toChar s) 
				else ('1', toChar $ mod s b)

plusM :: Maybe Char -> Maybe Char -> (Char, Char)
plusM = plusM' 32
				
(+++) :: Char -> Char -> (Char, Char)
(+++) = plus' 32


consolidateMinus' :: Char -> [(Char, Char)] -> [Char]
consolidateMinus' carry [] = if toInt carry > 0 then carry:'-':[] else []
consolidateMinus' carry ((x1,x2):xs) = let (y1,y2) = x2 <-> carry
										in case y2 of {
											'0' -> y2 : consolidateMinus' '1' xs
											_ -> y2 : consolidateMinus' '0' xs
										}

minus' :: Int -> Char -> Char -> (Char,Char)
minus' b x y = let s = (toInt x) - (toInt y)
				 in if s >= 0 then ('0' toChar s)
				 else (toChar $ (-s), '0')

(<->) :: Char -> Char -> (Char, Char)
(<->) = minus' 32

toInt :: Char -> Int
toInt c = snd $ head $ filter (\(a,b) -> a == c) charIntMap

toChar :: Int -> Char
toChar i = fst $ head $ filter (\(a,b) -> b == i) charIntMap

charIntMap :: [(Char, Int)]
charIntMap = [('0', 0),('1', 1),('2', 2),('3', 3),('4', 4),('5', 5),('6', 6),('7', 7),
              ('8', 8),('9', 9),('A', 10),('B', 11),('C', 12),('D', 13),('E', 14),('F', 15),
              ('G', 16),('H', 17),('I', 18),('J', 19),('K', 20),('L', 21),('M', 22),('N', 23),
              ('O', 24),('P', 25),('Q', 26),('R', 27),('S', 28),('T', 29),('U', 30),('V', 31),
              ('W', 32),('X', 33),('Y', 34),('Z', 35),('a', 36),('b', 37),('c', 38),('d', 39),
              ('e', 40),('f', 41),('g', 42),('h', 43),('i', 44),('j', 45),('k', 46),('l', 47),
              ('m', 48),('n', 49),('o', 50),('p', 51),('q', 52),('r', 53),('s', 54),('t', 55),
              ('u', 56),('v', 57),('w', 58),('x', 59),('y', 60),('z', 61),('!', 62),('@', 63)]

baseConversion :: Int -> Int -> [Char] -> [Char]
baseConversion b0 b1 (x:xs) = 
 
--First and last 9 digit
fib32 :: [Char] -> [Char] -> [[Char]]
fib32 first second = first : fib32 second (first <+> second)

fib32f :: [Char] -> [Char] -> [[Char]]
fib32f first second = first : fib32 second (first <++> second)

fib32n :: Int -> [Char]
fib32n n = reverse $ last $ take n $ fib32f "0" "1"

fib32' :: [[Char]]
fib32' = fib32 "0" "1"

-- m a -> ( a -> m b ) -> m b
-- ([Char], Int) -> ([Char] -> ([Char], Int)) -> ([Char], Int)
-- 

