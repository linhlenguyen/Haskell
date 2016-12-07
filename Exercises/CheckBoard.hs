module CheckBoard()
where
  --Check a tic tac toe board for winner

  --board :: [[Char]]
  --All rows, column and diagonals
  --Maybe list of list isn't a good data structure?

  rows :: (Eq a) => [[a]] -> [[a]]
  rows xs = xs

  toLL :: [a] -> [[a]]
  toLL [] = []
  toLL (x:xs) = [x] : toLL xs

  --Assuming the board is square NxN
  columns :: (Eq a) => [[a]] -> [[a]]
  columns [] = []
  columns [x] = (toLL x)
  columns xs = foldl foldf [] xs
    where foldf :: [[a]] -> [a] -> [[a]]
          foldf xs x = zipLL x xs

  zipLL :: [a] -> [[a]] -> [[a]]
  zipLL [] [] = []
  zipLL x [] = toLL x
  zipLL [] x = x
  zipLL (x:xs) (y:ys) = (x:y) : zipLL xs ys

  diagonals :: (Eq a) => [[a]] -> [[a]]
  diagonals xs = map mapf xs'
    where xs' = zip (diagonalIndexes $ length xs) xs
          mapf :: ([Int],[a]) -> [a]
          mapf (i,a) = filterWithIndexes i a

  filterWithIndexes :: [Int] -> [a] -> [a]
  filterWithIndexes i xs = filterWithIndexes' 0 i xs

  --Use Array?
  filterWithIndexes' :: Int -> [Int] -> [a] -> [a]
  filterWithIndexes' _ _ [] = []
  filterWithIndexes' index i (x:xs) = if (any (\k -> k == index) i) then x : filterWithIndexes' (index+1) i xs
                                      else filterWithIndexes' (index+1) i xs

  diagonalIndexes :: Int -> [[Int]]
  diagonalIndexes n = diagonalIndexes' n 0

  diagonalIndexes' :: Int -> Int -> [[Int]]
  diagonalIndexes' n i = if (i == n) then [] else [i,n-i-1] : diagonalIndexes' n (i+1)

  filterMod :: Int -> [a] -> [a]
  filterMod step xs = filterMod' step 0 xs

  filterMod' :: Int -> Int -> [a] -> [a]
  filterMod' step index (x:xs) = if ((mod index step) == 0) then x : filterMod' step (index+1) xs
                                 else filterMod' step (index+1) xs

  checkWin :: (Eq a) => [a] -> Bool
  checkWin [] = False
  checkWin [x] = True
  checkWin [x,y] = x == y
  checkWin (x:y:xs) = if (not $ x == y) then False else checkWin (y:xs)

  checkBoard :: (Eq a) => [[a]] -> Bool
  checkBoard xs = undefined
