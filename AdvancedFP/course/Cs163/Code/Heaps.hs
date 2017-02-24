module Heaps(Heap(..),newHeap,deleteMax,insertHeap,
              reHeapDown, reHeapUp, showHeap,
            LHeap,empty,isEmpty,insert,findMin,deleteMin)  where
     
import ArrCommands


-- In this file we give two implementations for heaps

-- 1) The first is command based and built on arrays. It implements
--    Max-heaps, where the root is always the maximal element

-- 2) The second is pure and is based upon leftist heaps. It implements
--    Min-heaps where the root is always the minimal element.


-------------------- Command based using arrays --------------

-- We have a logical Tree stored in an array
-- 1) Invariant: the array is 0 based
-- Then one can find the root of the tree, and the children
-- of each node in the tree, and the parent of each node in
-- the tree using the following formula

root = 0
left_child  i = i*2 + 1
right_child i = i*2 + 2
parent i | even i = (i `div` 2) - 1
         | odd i  = i `div` 2 

-- A heap is one of these array embedded trees, and a index
-- pointing to the last slot in the array, that represents a node 
-- in the tree. indexes larger than the last slot are unused.

data Heap a = Heap Int (Array a)

newHeap :: Int -> a -> IO (Heap a)
newHeap n a = 
  do { arr <- newArr (0,n) a
     ; return(Heap (-1) arr)
     }
     
-- Given an index and a Heap, find the index of the child 
-- with the largest value. Assume there is at least one child.

indexOfMaxChild :: (Ord t) => Int -> Heap t -> IO Int
indexOfMaxChild root (Heap last_slot arr)
    | left  >  last_slot = error "root has no valid children"
    | left  == last_slot = return left -- There is exactly one child
    | right <= last_slot =             -- Since right = (left+1) there must 
       do { leftv  <- readArr arr left -- be two children in this case
          ; rightv <- readArr arr right 
          ; return(if leftv >= rightv then left else right) }
  where left  = left_child root
        right = right_child root

-- Delete the maximal element. Do this by swapping
-- the root and last element, and shake the new root value
-- down the tree until it is in the correct place.
-- Be sure and record that the new tree is smaller by one
-- than the original tree.

deleteMax :: (Ord t) => Heap t -> IO (Heap t)        
deleteMax (Heap last arr) = 
  do { ans <- readArr arr root
     ; swap last root arr
     ; reHeapDown root (Heap (last-1) arr)
     ; return(Heap (last-1) arr) }

-- Shake the value down til it is in the correct place.

reHeapDown:: Ord a => Int -> Heap a -> IO()
reHeapDown root (Heap bottom arr) | (root*2 +1) > bottom = return ()
reHeapDown root (Heap bottom arr) = 
  do { max <- indexOfMaxChild root (Heap bottom arr)
     ; maxv <- readArr arr max
     ; rootv <- readArr arr root
     ; if maxv > rootv
          then do { swap root max arr; reHeapDown max (Heap bottom arr)}
          else return ()
     }

-- Insert a new element. Do this by inserting into
-- the array at the next position after last. Then
-- float this value up the tree until it is in the correct place.
-- Be sure and record that the new tree is larger by one
-- than the original tree.
     
insertHeap :: (Ord t) => t -> Heap t -> IO (Heap t)
insertHeap elem (Heap bottom arr) = 
  do { writeArr arr (bottom+1) elem
     ; reHeapUp (bottom+1) (Heap (bottom+1) arr)
     ; return(Heap (bottom+1) arr)}
     
-- float the "leaf" indexed value up the tree until 
-- it is in the correct place.

reHeapUp :: (Ord t) => Int -> Heap t -> IO ()
reHeapUp leaf (Heap bottom arr) | leaf==root = return ()
reHeapUp leaf (Heap bottom arr) =
  do { let par = parent leaf
     ; leafv <- readArr arr leaf
     ; parentv <- readArr arr par
     ; if leafv > parentv
          then do { swap leaf par arr; reHeapUp par (Heap bottom arr) }
          else return () }


swap n m arr =
  do { nv <- readArr arr n
     ; mv <- readArr arr m
     ; writeArr arr m nv
     ; writeArr arr n mv }  

-------- Pure using algebraic (data defined) trees ------------

-- Leftist Heaps are a pure implementation of Heaps
-- They are binary trees with the following invariants
-- 
-- 1) The Heap invariant. The value of Every Child is 
--    greater than the value of the parent. We are
--    building Min-heaps here. Use less than for Max-heaps
--
-- 2) The leftist invariant. The rank of every left-child
--    is equal to or greater than the rank of the 
--    cooresponding right-child. The rank of a tree is the
--    length of the right-most path.

data LHeap a = E | T Int a (LHeap a) (LHeap a)

-- Contracts for operations on leftists Heaps

empty:: (LHeap a)
isEmpty:: LHeap a -> Bool
insert:: Ord a => a -> LHeap a -> LHeap a
merge:: Ord a => LHeap a -> LHeap a -> LHeap a
findMin:: Ord a => LHeap a -> Maybe a
deleteMin:: Ord a => LHeap a -> Maybe(LHeap a)

------------------------------------------
-- operations on ranks

computeRank E = 0
computeRank (T r x a b) = 1 + computeRank b

-- Lookup the rank, rather than computing it.

rank E = 0
rank (T r _ _ _) = r

-------------------------------------------
-- operations on empty leftist heaps

empty = E

isEmpty E = True
isEmpty (T r v a b) = False

------------------------------------------------------------
-- Carefully combine two heaps and an element into a heap
-- that maintains the leftist property. Assume the element
-- "x" is larger than any element stored in "a" or "b"

makeT x a b = 
   if rank a >= rank b
      then T(rank b+1) x a b  -- a on the left since its rank is larger
      else T(rank a+1) x b a  -- b on the left since its rank is larger

-- Two heaps can be merged by combining their right sub-heaps.
-- Right sub-heaps are always smaller (the leftist invariant)
-- so there is always room to add elements down the right.

merge h E = h
merge E h = h
merge (h1@(T r1 x a1 b1)) (h2@(T r2 y a2 b2)) = 
  if x <= y  -- enforce the min-heap property
     then makeT x a1 (merge b1 h2)
     else makeT y a2 (merge h1 b2)

-------------------------------------------------
-- Other operations are easy once we have merge

insert x heap = merge (T 1 x E E) heap

        
findMin E = Nothing
findMin (T r x a b) = Just x

deleteMin E = Nothing
deleteMin (T r x a b) = Just(merge a b)






-----------------------------------------------------------
-- This code is for displaying heaps for the animations
-----------------------------------------------------------

zap :: String -> [String] -> [String] -> [String]
zap sep [] (y:ys) = (" "++sep++y) : zap sep [] ys
zap sep (x:xs) [] = (x++sep++" ") : zap sep xs []
zap sep [] [] = []
zap sep (x:xs) (y:ys) = (x++sep++y) : zap sep xs ys


--padl :: Int -> String -> String
padl n 0 s = replicate n ' ' ++ s
padl n w s = replicate (n-w-1) ' ' ++ "+" ++ replicate w '-' ++ s

--padr:: String -> Int -> String
padr s m 0 = s ++ replicate m ' '
padr s m w = s ++ replicate w '-' ++ "+" ++ replicate (m-w-1) ' '

make n s = replicate extra ' ' ++ s
  where m = length s
        extra = if m<n then(n-m) else 0

ok ' ' = True
ok '-' = True
ok '+' = True
ok _ = False

deltaL (" ":_) = 0
deltaL (x:xs) = length spaces
  where spaces = takeWhile ok (reverse x)

deltaR (" ":_) = 0
deltaR (x:xs) = length spaces
  where spaces = takeWhile ok x

-- invariant all string in the list have the same width

listChild :: Show a => Int -> Heap a -> IO (Int,[String])
listChild i (Heap bottom arr) | i > bottom = return (1,[" "])
listChild i (Heap bottom arr) = 
  do { let l = left_child i
           r = right_child i
     ; v <- readArr arr i
     ; (n,ls) <- listChild l (Heap bottom arr)
     ; (m,rs) <- listChild r (Heap bottom arr)
     ; let vs = (show v)
           p = length vs 
           new :: [String]
           new = zap (replicate p ' ') ls rs
     ; return(n+m+p,(padl n (deltaL ls) (padr vs m (deltaR rs))):new )}

showIndex last xs = putStrLn (str++ "  last = "++show last)
  where ys = zipWith f xs [0..length xs - 1]
        (ss,is) = unzip ys
        str = plistf id " " is " " " " ++ "   index\n" ++
              plistf id "[" ss "," "]" ++ "   values"
        f s i = (make n ss, make n is)
           where ss = show s
                 is = show i
                 n = max (length ss) (length is)

showHeap:: (Show t) => Heap t -> IO ()
showHeap (Heap n arr) | n < 0 = putStrLn "empty heap"
showHeap (Heap last arr) =
  do { (n,z) <- listChild 0 (Heap last arr)
     ; elems <- toListArr arr
     ; let nl s = s++"\n"
     ; putStrLn (concat (map nl z))
     ; showIndex last elems
     ; putStrLn "\n________________________________________\n"
     }
     
main =
  do { arr <- newListArr (0,20) [(0::Int)..20]
     ; showHeap (Heap 20 arr)
     }

plistf f op xs sep cl = op ++ help xs ++ cl
  where help [x] = f x
        help [] = ""
        help (x:xs) = f x ++ sep ++ help xs

-------------------------------------------------------
-- printing leftist heaps

lc E = (1,[" "])
lc (T _ v l r) = (n+m+p,(padl n (deltaL ls) (padr vs m (deltaR rs))):new )
  where (n,ls) = lc l
        (m,rs) = lc r
        vs = (show v)
	p = length vs 
	new :: [String]
	new = zap (replicate p ' ') ls rs

instance Show t => Show (LHeap t) where
  show h = "\n" ++ concat (map nl z)
    where nl s = s++"\n"
          (_,z) = lc h
          

add:: Char -> String -> LHeap Int -> LHeap Int
add 'a' s heap = insert (read s) heap
add 'd' s heap = j where (Just j) = deleteMin heap
add _ _ heap = heap
        
printH :: Show t => LHeap t -> IO ()        
printH heap = putStrLn(show heap)      

loop2 add print "" q0 = return ()
loop2 add print (e:more) q0 = 
    do { q1 <- add e more q0
       ; print q1
       ; next <- getLine
       ; loop2 add print next q1
       }

demoLHeap = 
    do { let h = (empty:: LHeap Int)
       ; printH h
       ; next <- getLine
       ; loop2 (\ x y z -> return(add x y z)) printH next h
     } 
     
demoHeap = 
    do { h <- newHeap 6 (-99)
       ; showHeap h
       ; next <- getLine
       ; loop2 add showHeap next h } 
 where add 'a' s heap = insertHeap (read s) heap
       add 'd' s heap = deleteMax heap
       add _ _ heap = return heap
     
