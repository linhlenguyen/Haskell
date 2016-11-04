{-# LANGUAGE ScopedTypeVariables #-}

import Data.SBV.Bridge.CVC4
import System.Random 
import Control.Monad (replicateM)

{-
Nate Launchbury, Fall 2014
Basic Commands:
  master : guess the computer's code
  mind   : computer guesses your code
  duel m : computer guesses computer's code for m games
-}

---------------------------------------------------------------------------------------

pegs = [0..9]::[Peg]
codeSize = 6 :: Int 
guesses  = 12 :: Int

type SPeg = SWord8
type Peg = Word8

peg :: SPeg -> SBool
peg x = x .< 10

type SCode = (SPeg,SPeg,SPeg,SPeg,SPeg,SPeg)
type Code  = (Peg,Peg,Peg,Peg,Peg,Peg)
code2list (a,b,c,d,e,f) = [a,b,c,d,e,f]
type SScore = (SWord8,SWord8)
type Score = (Word8, Word8)
type Table = [([SPeg], SScore)]

---------------------------------------------------------------------------------------

---------------------------------
-- Master
---------------------------------
-- guess the computer's code
master = do
  putStrLn ""
  putStringsLn ["I have selected a code of size ", show codeSize, " from ", show pegs]
  putStringsLn ["You have ", show guesses, " guesses"]
  code <- randomGuess -- this should be a Code
  n <- userGuess code 1
  if n <= guesses
  	then putStrLn "Well done"
  	else putStringsLn ["No more guesses. The code was ", show code]
   
userGuess :: [Peg] -> Int -> IO Int
userGuess code n
  | n > guesses = return n    -- this is failure
  | True = do
      g <- getGuess n
      if g==code then return n      -- this is success
      else do
      	   let (b,w) = unSym $ score (litcode code) (litcode g)
           putStringsLn ["That guess merits ", show b, " blacks and ", show w, " whites"]
           userGuess code (n+1)


getGuess :: Int -> IO [Peg]
getGuess n = do
  putStrLn ""
  putStrings ["Enter guess number ", show n, " : "]
  g <- getLine'
  let g' = [fromInteger $ read [c]|c<-g] 
  if checkcode g' then return g'
  else do
       putStrLn "That input was invalid. Try again."
       getGuess n

---------------------------------
-- Mind
---------------------------------
-- computer guesses your code
mind = do
  putStrLn ""
  putStringsLn ["Select a code of size ", show codeSize, " from ", show pegs]
  putStringsLn ["I have ", show guesses, " guesses"]
  n <- computerGuess [] 1
  if n==0
    then putStrLn "Hmmm. There are no possible solutions. I think you made a mistake."
    else if n <= guesses
    then putStrLn "I did it!"
    else putStringsLn ["No more guesses. I give up."]


computerGuess :: Table -> Int -> IO Int
--computerGuess [] n = return 0
computerGuess info n
  | n > guesses = return n  -- this is failure
  | True = do
      g <- --if n==1 then fmap Just randomGuess else 
        guess info
      case g of 
        Nothing -> return 0  -- this is failure
        Just c -> do
           (b,w) <- getGrade c n
           if b == fromIntegral codeSize
              then return n       -- this is success
           else
              let c' = litcode c in 
                 computerGuess ((c',(literal b,literal w)):info) (n+1)


getGrade :: [Peg] -> Int -> IO (Word8,Word8)
getGrade c n = do
  putStrLn ""
  putStrings ["My guess number ", show n, " is ", show c, " : "]
  bw <- getLine'
  if bw `elem` validScores
    then return (fromInteger $ read (take 1 bw), fromInteger $ read (drop 1 bw))
    else do
      putStrLn "That input was invalid. Enter two digits only. Try again."
      getGrade c n
 
---------------------------------
-- Duel
---------------------------------
-- computer guesses computer's code for m games
duel 0 = putStrLn ""
duel m = do 
  putStrLn ""
  putStringsLn ["Code of size ", show codeSize, " from ", show pegs]
  putStringsLn [ show guesses, " guesses"]
  code <- randomGuess
  n <- duelGuess code [] 1
  if n==0
    then putStringsLn ["No solutions. There was a mistake. The code was ", show code]
  else if n <= guesses
    then putStrLn "Completed"
  else putStringsLn ["No more guesses. The code was ", show code]
  duel (m-1)

duelGuess :: [Peg] -> Table -> Int -> IO Int
duelGuess real info n 
   | n > guesses = return n  -- this is failure
   | True = do
      g <- if n==1 then fmap Just randomGuess else 
        --guess' info 
        diverseGuess info  
      case g of 
        Nothing -> return 0  -- this is failure
        Just c -> do
           let (b,w) = score (litcode c) (litcode real)
           let (b',w') = unSym (b,w)
           putStringsLn ["Guess ", show n, " : ", show c, " : ", show b', show w']
           if b == fromIntegral codeSize
              then return n       -- this is success
           else 
              let c' = litcode c in 
                duelGuess real ((c',(b,w)):info) (n+1)


---------------------------------
-- SMT
---------------------------------

-- Guesses a code which meets the scores given in the Table
guess' :: Table -> IO (Maybe [Peg])
guess' xs = do
   res <- sat $ \t -> let g = code2list t in 
            scoreList g xs &&& iscode g
   return (extractModel res :: Maybe [Peg])

-- Functionally the same as guess' 
guess :: Table -> IO (Maybe [Peg])
guess xs = predicateGuess (\x -> scoreList x xs) xs


-- Allows the use of a predicate as input to the guess 
-- These predicates are heuristics which will be disregarded if nothing comes of them 
predicateGuess :: ([SPeg] -> SBool) -> Table -> IO (Maybe [Peg])
predicateGuess p xs = do
   res <- sat $ \t -> let g = code2list t in 
            scoreList g xs &&& iscode g &&& p g
   case (extractModel res :: Maybe [Peg]) of
     Just pegs -> return (Just pegs)
     Nothing   -> guess' xs


-- Example of allSat (not actually used)
-- Type 'codes' but be ready to interrupt
codes = do
   res <- allSat $ \t -> let g = code2list t in 
            iscode g
   return res

---------------------------------
-- Heuristic predicates
---------------------------------

-- Heuristic: Guess consists of all unique pegs except for one pair
diverseGuess xs = predicateGuess (\x-> diverse x) xs

diverse :: [SPeg] -> SBool
diverse x = diversity x .== fromIntegral (codeSize - 1)

diversity :: [SPeg] -> SWord8
diversity []     = 0
diversity (x:xs) = diversity xs + ite (x `elt` xs) 0 1



---------------------------------
-- Scoring 
---------------------------------

-- Scores a list of SPegs against another list (the actual code)
score :: [SPeg] -> [SPeg] -> SScore
score xs ys = (b,bw - b)
  where 
        b  = sum $ zipWith equal xs ys 
        bw = match (sort xs) (sort ys) 

match :: [SPeg] -> [SPeg] -> SWord8
match [] _ = 0
match _ [] = 0
match (x:xs) (y:ys) = ite (x.==y) (1 + match xs ys) 
                          (ite (x.<y) (match xs (y:ys)) (match (x:xs) ys))

-- Checks whether or not a code matches the scores of all previous guesses
scoreList :: [SPeg] -> Table -> SBool
scoreList x [] = true
scoreList x ((y,s):ys) = score x y .== s &&& scoreList x ys


---------------------------------
-- Utility Functions
---------------------------------

-- Selects a random element from the list
randomFrom :: [a] -> IO a
randomFrom xs = do
  r <- randomIO :: IO Float
  return (xs !! truncate (r * fromIntegral (length xs)))

putStrings   xs = putStr (concat xs)
putStringsLn xs = putStrLn (concat xs)


(<|||>) :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
first <|||> second = do
  x <- first
  case x of
    Just y  -> return x
    Nothing -> do putStrLn "... next predicate"  -- comment this out for silent handoff
                  second

elt x []     = false
elt x (y:ys) = x .== y ||| elt x ys

randomGuess = replicateM codeSize (randomFrom pegs)

litscore (b,w) = (literal b, literal w)
litcode c = map literal c 

validScores = [show b ++ show w | b <- [0..codeSize], w <- [0..cap b]]
  where cap b = if b == codeSize-1 then 0 else codeSize - b 


unSym :: SScore -> (Word8, Word8)
unSym (x,y) = (unJust $ unliteral x, unJust $ unliteral y)
   where unJust (Just x) = x

checkcode :: [Peg] -> Bool
checkcode xs = length xs == codeSize && and [x `elem` pegs | x <- xs]

iscode :: [SPeg] -> SBool
iscode xs = bAnd $ map peg xs

equal :: SPeg -> SPeg -> SWord8
equal x y = ite (x .== y) 1 0

blacks :: [SPeg] -> [SPeg] -> SWord8
blacks [] [] = 0
blacks xs ys = sum $ zipWith equal xs ys

---------------------------------
-- Sorting a list of SWord8s
---------------------------------

sort :: [SWord8] -> [SWord8]
sort []     = []
sort (x:xs) = insert x (sort xs)

insert :: SWord8 -> [SWord8] -> [SWord8]
insert x []     = [x]
insert x (y:ys) = ite (x .<= y) (x:y:ys) (y:insert x ys)

-- proof of the above function for length 4
proveSort4 = prove $ \a b c d -> sorted (sort [a,b,c,d])
  where
    sorted [a,b,c,d] = a.<=b &&& b.<=c &&& c.<=d


-----------------------------------------------------------------
-- a fix for the ghci getLine bug on macs where DEL doesn't work
-----------------------------------------------------------------

getLine' :: IO String
getLine' = forward []

forward xs = do
  c <- getChar
  case c of
    '\n'   -> return (reverse xs)
    '\DEL' -> backward xs
    x      -> forward (x:xs)

backward []     = del >> del >> forward []          -- remove "^?" at the start of input
backward (x:xs) = del >> del >> del >> forward xs   -- remove "^?" and then the character

del = putStr "\ESC[D \ESC[D" -- Magic character sequence for overwriting with a space




