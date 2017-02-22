-- David Pouliot - CS510 Logic Programming - Project

module ParseLine where


import Text.Printf
import Data.Tuple.Select
import Data.String.Utils

-- part of this code is from 
-- http://rosettacode.org/wiki/Parsing/Shunting-yard_algorithm#Haskell
-- but I did a lot of modifications to that code.
 
prec "=" = 1 
prec "^" = 4
prec "*" = 3
prec "/" = 3
prec "+" = 2
prec "-" = 2
 
leftAssoc "^" = False
leftAssoc _ = True
 
isOp (t:[]) = t `elem` "-+/*^="
isOp _      = False
 
simSYA xs = final ++ [lastStep]
  where final = scanl f ([],[],"") xs
        lastStep = (\(x,y,_) -> (reverse y ++ x, [], "")) $ last final
        f (out,st,_) t | isOp t =
                         (reverse (takeWhile testOp st) ++ out
                         , (t:) $ (dropWhile testOp st), t)
                       | t == "(" = (out, "(":st, t)
                       | t == ")" = (reverse (takeWhile (/="(") st) ++ out,
                                     tail $ dropWhile (/="(") st, t)
                       | True     = (t:out, st, t)
          where testOp x = isOp x && (leftAssoc t && prec t == prec x
                                      || prec t < prec x)
 
main = do
    a <- getLine   
    printf "%30s%20s%7s" "Output" "Stack" "Token"
    mapM_ (\(x,y,z) -> printf "%30s%20s%7s\n" 
            (unwords  x) (unwords y) z) $ simSYA $ words a
-- end of code from source listed above

test =  simSYA $ words "x = 5 + 4 + 9 / 7 * 33"

test2 = sel1 (last test)

test3 = addPar (sel1 ( last (simSYA $ words "5 + 4 + 9 / 7 * 33")))

test4 = prefix "5 + 4 + 9 / 7 * 33"

test5 = prefix "x = 5 + 4 + 9 / 7 * 33"

test6 = (prefix "x = 5 * 4 + 9 / 7 * 33 - 9 / 4 + 7")

test7 = prefix "index = 5 - 2"

test8 = specialReverse (words test7) []

prefix x = addPar (sel1 (last (simSYA $ words x))) [0] []

temptest x =  (sel1 (last (simSYA $ words x)))

--test8 = temptest "index = 5 - 2"
test9 = specialReverse test8 []
test91 = reverse test8

test92 = specialReverse (reverse test2) []
-- for n need to keep a list of counts, when adding a ) increase the value
-- of the previous n on the list


-- end of tests, beginning of code

addPar [] [] result = join " " (init (reverse result ))
addPar [] n result = join " " (init( reverse ((addp n []) ++ result) ))
addPar (x:xs) (n:ns) result
	| isOp x = addPar xs (0:n:ns) (x:"(":result)
	| n == 1 = addPar xs (popns ns) ((addp (n:ns) [])++x:result)   -- need to recursively check for each n == 1
	| otherwise = addPar xs ((n + 1):ns) (x:result)
	
-- need function to pop off all ones from ns
popns [] = []
popns (n:ns)
	| n == 1 = popns ns
	| otherwise = ((n+1):ns)

-- need function to add ) until see zero from ns
addp [] p = p
addp (n:ns) p
	| n == 1 = addp ns (")":p)
	| otherwise = p
	

-- this is a work in progress and not used at the moment
specialReverse [] y = y
specialReverse (x:xs) y
	| isOp x = specialReverse xs (x:y)
	| otherwise = specialReverse (remNonOps (x:xs)) (words( getNonOps (x:xs) [] ))++y -- modify

isP (t:[]) = t `elem` "()"
isP _      = False

remNonOps [] = []
remNonOps (x:xs) 
	| isOp x = (x:xs)
	| isP x = (x:xs)
	| otherwise = remNonOps xs

getNonOps [] [] = []
getNonOps [] y = y
getNonOps (x:xs) [] 
	| isOp x = []
	| isP x = []
	| otherwise = getNonOps xs x
getNonOps (x:xs) y
	| isOp x = y
	| isP x = y
	| otherwise = getNonOps xs (y++" " ++x)
	
	--	| otherwise = getNonOps xs (foldr (:) (x:" ") y)
	
	
-- | otherwise = getNonOps xs (y++x)

{-
addPar [] n result = reverse result
addPar (x:xs) n result
	| isOp x = addPar xs 0 (x:"(":result)
	| n == 1 = addPar xs 0 (")":x:result)
	| otherwise = addPar xs (n + 1) (x:result)

-}
