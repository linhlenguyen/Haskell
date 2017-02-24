module ManySorts  where

import ArrCommands

--------------------------------------------------------
-- http://en.literateprograms.org/Insertion_sort_(Haskell)

insert elem [] = [elem]
insert elem (item : xs) | elem == item  = elem : item : xs
insert elem (item : xs) | elem < item   = elem : item : xs
insert elem (item : xs) | elem > item   = item: (insert elem xs)


sortInsert :: Ord a => [a] -> [a]
sortInsert [] = []
sortInsert (x:xs) =  insert x (sortInsert xs)

---------------------------------------------------

bubble lhs [x] = (lhs,x)
bubble lhs (x:y:zs) | x <= y = bubble (lhs ++ [x]) (y:zs)
                    | x >  y = bubble (lhs ++ [y]) (x:zs)

bubbleSort [] = []
bubbleSort xs = iterate xs []
  where iterate [] ans = ans
        iterate xs ans = iterate rest (biggest : ans)
           where (rest,biggest) = bubble [] xs

-----------------------------------------------------

exchange (x:xs) [] = (x,xs)
exchange (x:xs) (y:ys) | x <= y = exchange (x:xs ++ [y]) ys
                       | x > y  = exchange (y:xs ++ [x]) ys
                       
exchangeSort [] = []
exchangeSort xs = iterate xs
  where iterate [] = []
        iterate (x:xs) = smallest : iterate rest
           where (smallest,rest) = exchange [x] xs  
           
split xs = (take n xs,drop n xs)
  where n = (length xs) `div` 2

                    
---------------------------------------------------
-- http://en.literateprograms.org/Quicksort_(Haskell)
-- http://en.wikipedia.org/wiki/Quicksort

quikSort [] = []
quikSort [x] = [x]
quikSort (pivot:xs) = (quikSort smaller) ++ [pivot] ++ (quikSort larger)
  where smaller = filter (<= pivot) xs
        larger  = filter (> pivot) xs
        
--------------------------------------------------

bucketSort :: (Int, Int) -> [Int] -> IO [Int]
bucketSort (low,high) xs =
  do { arr <- newArr (low,high) []
     ; let moveEach [] = return ()
           moveEach (x:xs) = 
             do { others <- readArr arr x
                ; writeArr arr x (x:others)
                ; moveEach xs }
     ; moveEach xs
     ; listOfLists <- toListArr arr
     ; return(concat listOfLists)
     }

---------------------------------------------------------
-- http://en.literateprograms.org/Counting_sort_(Haskell)

countingSort :: (Int, Int) -> [Int] -> IO [Int]
countingSort (low,high) xs =
  do { arr <- newArr (low,high) 0
     ; printArr arr
     ; let countEach [] = return ()
           countEach (x:xs) = 
             do { n <- readArr arr x
                ; writeArr arr x (n+1)
                ; printArr arr
                ; countEach xs }
     ; countEach xs
     ; listOfcounts <- toListArr arr
     ; return(makeReal low listOfcounts)
     }

makeReal n [] = []
makeReal n (0 : cs) = makeReal (n+1) cs     
makeReal n (c:cs) = replicate c n ++ (makeReal (n+1) cs) 

------------------------------------------------------
-- http://en.wikipedia.org/wiki/Radix_sort