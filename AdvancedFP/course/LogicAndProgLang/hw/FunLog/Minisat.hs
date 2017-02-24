module Minisat(putClauses,solveClauses,negatedCl
              ,solveWithMinisat,cycleMinisat,minisat
              ,chooseMinisat,firstMinisat) where

import Prop 
-- import CNF
import System.IO(IOMode(..),openFile,hClose,hPutStrLn)
import System.IO(fixIO,hClose,hFlush,stdout,stdin,hGetLine)
import System.Cmd(rawSystem,system)

readLine prompt = (do {putStr prompt; hFlush stdout; hGetLine stdin})

-- YOU MUST CHANGE THIS SO IT POINTS TO MINISAT ON YOUR MACHINE.

minisat = "C:\\cygwin\\bin\\minisat.exe"

------------------------------------------------------------------
-- This function writes a [[Prop a]] representing a formula
-- in CNF to a file in a format that can be read by the minisat
-- SAT-solver. The [[Prop a]] must contain only LetterP and Negated LetterP


-- putClauses:: (Ord a, Show a,Num a) => FilePath -> [[Prop a]] -> IO ()
putClauses file xs =
  let vars xs = (maximum0 . concat . concat) (map (map letters) xs)
      smallest = minimum0 (concat(concat(map (map letters) xs)))
      showClause xs = f xs
        where f [] = " 0"
              f [x] = showLit x++" 0"
              f (x:xs) = showLit x++" "++f xs
      maximum0 [] = 1
      maximum0 xs = maximum xs
      minimum0 [] = 1
      minimum0 xs = minimum xs
      showLit (LetterP x) = show x
      showLit (NotP(LetterP x)) = "-"++show x
      showLit v = error ("Nonliteral in showLit:" ++ show v)
  in do { if smallest == 0 
             then error ("In putClauses: A Variable is named '0', this has special meaning in a DIMACS *.cnf file.")
             else (return ())
        ; putStrLn("Minisat input file is: "++file)
        ; h <- openFile file WriteMode
        ; hPutStrLn h ("p cnf "++show (vars xs)++" "++show (length xs))
        ; mapM_ (hPutStrLn h . showClause) xs
        ; hClose h }

readMinisatResult:: FilePath -> IO (Maybe [Int])
readMinisatResult resultfile =
  do { result <- readFile resultfile
     ; case lines result of
        "SAT" : xs : _ -> do
           return (Just (takeWhile (/= 0) (map read(words xs))))
        _ -> return $ Nothing
     }


firstMinisat:: ([Int] -> IO a) -> FilePath -> FilePath -> [[Prop Int]] -> IO a
firstMinisat action cnfName solName clauses =
  do { ans <- solveClauses cnfName solName clauses
     ; case ans of
         Nothing -> error ("Minisat fails, no solution")
         Just sol -> 
           do { putStrLn ("Solution is\n   "++show sol)
              ; action sol } }

chooseMinisat:: Show a => ([Int] -> IO(a,b)) -> FilePath -> FilePath -> [[Prop Int]] -> IO b
chooseMinisat action cnfName solName clauses = 
        do { ans <- solveClauses cnfName solName clauses
           ; case ans of
               Nothing -> error ("Minisat fails to find an answer")
               Just sol -> do { let newcls = negatedCl sol : clauses
                              ; (next,ans) <- (action sol)
                              ; x <- readLine ("Solution\n"++show next++"Keep this one? (<enter> = no, anything else is yes)")
                              ; case (x::String) of
                                 [] -> putStrLn "\n###ChooseMinistat\n" >> chooseMinisat action cnfName solName newcls
                                 _ -> return ans} }



solveClauses:: FilePath -> FilePath -> [[Prop Int]] -> IO (Maybe [Int])
solveClauses cnfName solName clauses = 
  do { putClauses cnfName clauses
     ; rawSystem minisat [cnfName,solName]
     ; readMinisatResult solName }

solveWithMinisat:: FilePath -> FilePath -> [Prop Int] -> IO (Maybe [Int])
solveWithMinisat cnfName solName constraints =
  do { let clauses = concat (map cnf constraints)
     ; solveClauses cnfName solName clauses }

               
cycleMinisat:: a -> ([Int] -> IO a) -> FilePath -> FilePath -> [Prop Int] -> IO a
cycleMinisat defvalue action cnfName solName constraints =
  do { let clauses = concat (map cnf constraints)
     ; ans <- solveClauses cnfName solName clauses
     ; loop 1 defvalue action cnfName solName clauses ans
     }
     
loop n defvalue action cnfName solName clauses Nothing = 
  do { putStrLn "No more solutions"; return defvalue }
loop n defvalue action cnfName solName clauses (Just sol) = 
  do { putStrLn ("Solution "++show n)
     ; ans <- action sol
     ; s <- readLine ("More solutions? <return> for more")
     ; case s of
         ("") -> do { let newcl = (negatedCl sol : clauses)
                       -- ; putStrLn("NEW=\n"++show (head newcl))
                       ; ans <- solveClauses cnfName solName newcl
                       ; loop (n+1) defvalue action cnfName solName newcl ans }     
         ('q':_) -> do { putStrLn "Stopped by user."; return ans}
         other -> do { putStrLn ("Unknown command: "++show other); return ans}
         }


negatedCl :: [Int] -> [Prop Int]
negatedCl xs = map f xs
  where f x | x<0 = (LetterP (abs x))
        f x = NotP(LetterP (abs x))
 