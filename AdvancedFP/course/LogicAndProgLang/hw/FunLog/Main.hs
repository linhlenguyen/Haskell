module Main where


-- C:/ghc/ghc-7.0.1/bin/ghc-pkg.exe list
-- C:/ghc/ghc-7.0.1/bin/ghc.exe  -auto-all -o funlogic --make main

-- Optimization on  -O2
-- cd d:/work/sheard/Courses/LogicAndProgLang/web/hw/FunLog2
-- C:/HP2013.2.0.0/bin/ghc.exe -O2 -auto-all -o funlogic --make main
-- ./funlogic.exe ../funlog/tests/test3.funlog

-- Profiling commands
--  cd d:/work/sheard/Courses/LogicAndProgLang/web/hw/FunLog2
--  C:/HP2013.2.0.0/bin/ghc.exe -auto-all -o funlogic -fprof-auto -prof --make main
-- ./funlogic.exe ../funlog/tests/test3.funlog +RTS -p
-- 

 -- to hide the 3.1.1 vwrsion of parsec, type at the cygwin prompt
-- ghc-pkg hide parsec-3.1.1

import System.Environment(getArgs)
import qualified Control.Exception as Ex
import Syntax  -- hiding (evalExpr) --(Prog(..),env1,evalDec,evalExpr)
import Parser --(parseFile,program,parse2,expr)
import Auxfuns(plistf)
import Value (Value(..),NonStandard(..),typeNameVal)
import Eval
import FiniteSet (SetI(..))
import UniqInteger(resetnext)
import Data.IORef(newIORef,readIORef,writeIORef,IORef)
import System.IO.Unsafe(unsafePerformIO)

import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($$),($+$),render)
-- import FastBDD(p2b)
import Graphviz
import GHC.IO.Handle(hFlush)
import GHC.IO.Handle.FD(stdout)
import Monads(readLine)
import Data.Char(chr)
import Cnf
-------------------------------------------------------------------------

data Mode = EXP | FORM | CONSTRAINT

work = run "tests/work.funlog"

testprod = run "tests/Product.funlog"
testp = run "tests/paper.funlog"
testq = run "tests/queensSMT.funlog"
testsmt = run "tests/smt.funlog"
testmap = run "tests/MapColoring.funlog"
testsmall = run "tests/smallMapColoring.funlog"
testdata = run "tests/AlgData.funlog"
testsoduko = run "tests/Soduko.funlog"
testsoduko2 = run "tests/Soduko2.funlog"
testsoduko3 = run "tests/Soduko3.funlog"
testlong = run "tests/SodukoLong.funlog"
testfun = run "tests/Functions.funlog"
teste = run "tests/empty.funlog"
testform = run "tests/formula.funlog"
testqueen = run "tests/queens.funlog"
smoke = run "tests/Smokes.funlog"
bin = run "tests/binPacking.funlog"
test2 = run "tests/test2.funlog"
test3 = run "../funlog/tests/test3.funlog"
test4 = run "tests/smallQueenSat.funlog"

version = "1.0"

main :: IO()
main =
  do { args <- getArgs
     ; putStr ("\nFunlogic version "++version++"\n")
     ; putStr "Type ':?' for command line help.\n"
     ; case args of
        ( file : _) -> run file
        x -> putStrLn ("Unknown command line args"++show x)       
     }

 
unM x = x

run :: FilePath -> IO ()
run name =
  do { (Prog ds,finalState) <- parseFile program name -- 
     ; putStrLn(show (Prog ds))
     ; resetnext firstNewInteger
     ; env2 <- unM (evalDecs ds (VEnv (1,env1)))
     ; loop2 (finalState,env2,EXP)
     }

prompt EXP = "exp>"
prompt FORM = "form>"
prompt CONSTRAINT = "constraint>"

evalDecs :: [Decl] -> VEnv -> IO VEnv
evalDecs [] env = return env
evalDecs (d:ds) env = 
  putStrLn (decName d) >> evalDecC env d (\ env2 -> evalDecs ds env2)

action :: LoopState -> [Char] -> IO [Char]
action (env@(finalParsState@(gg),envx,EXP)) s =
  do { exp <- (parseAll finalParsState "keyboard input for <expr>" expr s)
     ; v <- evalC exp envx return
     ; putStrLn(show v++":: "++typeNameVal v)
     ; return "" }
action  (env@(finalParsState,envx@(VEnv(nextpropvar,zs)),FORM)) s =
  do { form <- (parseAll finalParsState "keyboard input for <formula>" formulaP s)  
     ; (shape,set) <- evalForm envx form   
     ; return $ plistf fst "[" shape "," "]"++"\n"++show set }
action (env@(finalParsState,envx,CONSTRAINT)) s =
  do { c <- parseAll finalParsState "keyboard input for <constraint>" constraint s
     ; v <- evalCon c envx 
     ; putStrLn(show v)
     ; case v of
        -- VNS(NSProp x) -> pnG(p2b x) >> return ()
        _ -> return ()         
     ; return "" }

switch (finalParsState,envx,mode) m = (finalParsState,envx,m)

type LoopState = (InternalState,VEnv,Mode)

loop2 :: LoopState -> IO ()
loop2 (env@(finalParsState,envx,mode)) = Ex.catches 
   (do { s <- readLine (prompt mode++" ")
       ; case s of
           ":q" -> return ()
           ":e" -> loop2 (switch env EXP)
           ":f" -> loop2 (switch env FORM)
           ":c" -> loop2 (switch env CONSTRAINT)
           ":?" -> putStrLn ":q quit\n:e move to expression evaluation mode\n:f move to the formula evaluation mode\n:c move to the constraint evaluation mode\n:n show environment\nx evaluate x in current mode\n" >>
                   loop2 env
           ":n" -> putStrLn(show envx) >>
                   loop2 env
                      
           "" -> loop2 env
           _  -> do { string <- Ex.catches (action env s) 
                                [Ex.Handler (\ (Ex.ErrorCall s)-> return s)]
                    ; putStrLn string 
                    ; loop2 env }
       }) handlers
  where catchError (Ex.ErrorCall s) = putStrLn s >> loop2 env
        handlers = [Ex.Handler catchError]

     