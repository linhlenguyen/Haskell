module ListPermutation(
findPerm
)
where
  --Design an algorithm to print all permutations of a string. For simplicity, assume all
  --characters are unique

  --getPermutation :: [a] -> [[a]]
  --getPermutation [x,y] = [[x,y],[y,x]]
  --getPermutation (x:xs) =
  --take x from the list as head and recursively build permutation from sublist excluding x
  --base case for 2 items is already defined

  listTake' :: [a] -> [a] -> [(a,[a])]
  listTake' _ [] = []
  listTake' s (x:xs) = (x,(s ++ xs)) : listTake' (s ++ [x]) xs

  listTake :: [a] -> [(a,[a])]
  listTake = listTake' []

  findPermutation :: [a] -> [[a]]
  findPermutation [] = []
  findPermutation [x] = [[x]]
  findPermutation [x,y] = [[x,y], [y,x]]
  findPermutation ls@(x:xs) = concat $ map mapf $ listTake ls
    where mapf :: (a, [a]) -> [[a]]
          mapf (x,ls) = map (\pl -> x:pl) $ findPermutation ls

  --Another solution
  insertA :: [a] -> [a] -> [[a]]
  insertA x [] = [x]
  insertA x ls@(y:ys) = (x ++ ls) : insertA (y:x) ys

  findPerm :: [a] -> [[a]]
  findPerm [] = []
  findPerm [x] = [[x]]
  findPerm [x,y] = [[x,y],[y,x]]
  findPerm (x:xs) = concat $ map (insertA [x]) $ (findPerm xs)

  anyDuplicate :: (Eq a) => [a] -> Bool
  anyDuplicate [x] = False
  anyDuplicate (x:xs) = any (\k -> k == x) xs || anyDuplicate xs
