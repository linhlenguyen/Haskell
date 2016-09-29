--Cardiano Triplets
--Make this work with Floating point numbers!

power :: (Integral a) => a -> a -> a
power 1 a = a
power p a = a * power (p - 1) a 

root' :: (Integral a) => a -> a -> a -> Maybe a
root' r n a 
			| npr == a = Just n
			| npr > a = Nothing
			| otherwise = root' r (n+1) a
			where npr = power r n

root :: (Integral a) => a -> a -> Maybe a
root r a = root' r 1 a

cube :: (Integral a) => a -> a
cube = power 3 

cubert :: (Integral a) => a -> Maybe a
cubert = root 3

mPlus :: (Integral a) => Maybe a -> Maybe a -> Maybe a
mPlus (Just a) (Just b) = Just (a + b)
mPlus _ _ = Nothing

cardinoPart1 :: (Integral a) => a -> a -> a -> Maybe a
cardinoPart1 a b c = cubert (a + ((*b) $ truncate (sqrt $ fromIntegral c)))

cardinoPart2 :: (Integral a) => a -> a -> a -> Maybe a
cardinoPart2 a b c = cubert (a - ((*b) $ truncate (sqrt $ fromIntegral c)))

cardinoFnc :: (Integral a) => a -> a -> a -> Maybe a
cardinoFnc a b c = mPlus (cardinoPart1 a b c) (cardinoPart2 a b c)

isCardinoTriplets :: (Integral a) => a -> a -> a -> Bool
isCardinoTriplets a b c = case cardinoFnc a b c of {Just 1 -> True;
													_ -> False;
													}
												
--Tidying up
shufflef :: (Integral a) => a -> a
shufflef a = mod (2*a^3 - 9*a^2 + 8*a + 27) 10

--Given a hash number and a list, shuffle the list in arbitrary order
rfn :: Int -> Int -> Int
rfn ln a = mod (2*a^4 + 8*a^2 - 7*a + 11) ln

shuffle' :: (Ord a) => [a] -> Int -> (Int -> Int) -> [a]
shuffle' ls i f = if i > length' ls then ls else shuffle' (swapItems i (f i) ls) (i+1) f

shuffle :: (Ord a) => [a] -> (Int -> Int) -> [a]
shuffle ls f = shuffle' ls 0 f

length' :: [a] -> Int
length' x = foldl (\s x -> s + 1) 0 x 

hasValueAt :: Int -> [a] -> Bool
hasValueAt n xs = if (length' xs) <= n then True else False

valueAt :: Int -> [a] -> Maybe a
valueAt _ [] = Nothing
valueAt n (x:xs) = if n == 0 then Just x else valueAt (n-1) xs 

updateList :: Int -> a -> [a] -> [a]
updateList _ _ [] = []
updateList n v (x:xs) = if n == 0 	then v : (updateList (n - 1) v xs)
									else x : (updateList (n - 1) v xs)

updateMaybe :: Int -> Maybe a -> [a] -> [a]
updateMaybe _ Nothing ls = ls
updateMaybe n (Just x) ls = updateList n x ls

swapItems :: Int -> Int -> [a] -> [a]
swapItems i0 i1 ls = (let v0 = valueAt i0 ls;		 
						 v1 = valueAt i1 ls		 
						 in (let ls1 = updateMaybe i0 v1 ls 		
							in updateMaybe i1 v0 ls1))
						
--swapItemInList' :: Int -> Int -> [a] -> Maybe [a]
--swapItemInList' i0 i1 x = swapItemInList i0 i1 0 x

--State Monad implementation
--swapItemInList'' :: Int -> Int -> State [a]()
--swapItemInList'' i0 i1 = State $ \x:xs ->  ( ,())

--stackPush :: a -> State (Stack a) ()
--stackPop :: State (Stack a) a

factorial :: (Integral a) => a -> a
factorial 1 = 1
factorial x = x * factorial (x - 1)

maplf :: [(a -> b)] -> [a] -> [b]
maplf [] _ = []
maplf _ [] = []
maplf (f:fs) (x:xs) = (f x) : maplf fs xs

