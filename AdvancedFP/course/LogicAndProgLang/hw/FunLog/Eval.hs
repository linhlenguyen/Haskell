{-# LANGUAGE PatternGuards, RankNTypes, GADTs, FlexibleContexts #-}

module Eval where

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($$),($+$),render)
import Control.Monad(foldM)
import qualified MyMap as DM
import System.IO(fixIO)
import Data.Array(listArray,array)
import Data.List(elemIndex)
import Data.IORef(newIORef,readIORef,writeIORef,IORef)
import System.IO.Unsafe(unsafePerformIO)
import Text.ParserCombinators.Parsec.Pos(SourcePos)
import Data.Ratio((%),numerator,denominator)

import Auxfuns (plistf,lift1M,whenM)
import UniqInteger
import Boolean
import Literal
import Value
import Syntax
import Dimension
import FiniteSet    
import Parser(parse2,expr,formulaP)
import Minisat
import Prop(Prop(LetterP,NotP),cnf2,andL,andOpt,orL,orOpt,toProp,propStat)
import Cnf
import Monads(noPos,foldrM,maybeM,when)
import YicesSyntax
import YicesPipe(runYices,ResY(..))
import MathProg(MExp(..),Rel(..),fillProb,shRng
               ,mpsPrint,solveMPS,plusM,minusM,timesM,ltM,lteqM,gteqM,gtM,eqM)


-------------------------------------------------------------
valueToPat e (VBase l) = (PLit noPos l)
valueToPat e (VTuple vs) = PTuple TUPLE (map (valueToPat e) vs)
valueToPat e (VCon n typ c vs) = PCon (Nm(c,noPos)) (map (valueToPat e) vs)
valueToPat e v = error ("Escaped expression in formula pattern:\n   "++
                        show e++"\nis not a simple value:"++"\n   "++show v)

type Sub = [(Name,Value)]


-----------------------------------------------------------
-- When evaluating Expresions we match Pat against values

match:: Sub -> Pat -> Value -> Maybe (Sub)
match s p v = help s p v where
  help s (PVar nm) v = Just((nm,v):s)
  help s (PWild pos) v = Just s
  help s (PLit pos c) (VBase d) = if c==d then Just s else Nothing
  help s (PTuple TUPLE ps) (VTuple vs) | (length ps==length vs)= thread s ps vs
  help s (PTuple DIM ps) (VDomain ds) 
       | (length ps==length ds)
       = thread s ps (map (\ d -> VDomain [d]) ds)
  help s (PCon (Nm(":",_)) [x,y]) (VBase(LString (z:zs))) = 
    do { s1 <- match s x (VBase(LChar z))
       ; match s1 y (VBase(LString zs)) }
  help s (PCon (Nm("[]",_)) []) (VBase(LString (""))) = Just s    
  help s (PCon (Nm(c,_)) ps) (VCon n typ d vs) | (c==d) = thread s ps vs
  help s (p@(PEsc e)) v = error ("Escaped pattern in non formula\n   "++show p)
  help s p v = Nothing

thread :: Sub -> [Pat] -> [Value] -> Maybe (Sub)
thread s [] [] = Just s
thread s (p:ps) (v:vs) = 
   case match s p v of
     Just s' -> thread s' (map (substPat s') ps) vs
     Nothing -> Nothing
     

substPat:: Sub -> Pat -> Pat
substPat [] (PVar nm) = PVar nm
substPat ((x,VBase v):s)       (PVar nm) | (x == nm) = PLit (loc nm) v
substPat ((x,VCon n typ c []):s) (PVar nm) | (x == nm) = PCon (Nm(c,loc nm)) []
substPat ((x,v):s)             (PVar nm) | (x == nm) = error ("Non literal in substPat: "++show v)
substPat (_:s) (v@(PVar nm)) = substPat s v
substPat s (PWild p) = PWild p
substPat s (PLit p c) = PLit p c
substPat s (PTuple x ps) = PTuple x (map (substPat s) ps)
substPat s (PCon c ps) = PCon c (map (substPat s) ps)
substPat s (PEsc e) = error "No substPat yet"

        

toValues:: FiniteSet a -> [(a,[Value])]
toValues (FA ds set) = map f (DM.toList set) -- (map VBase . flatIndexToLits ds . fst) (toList set)
  where f (key,bool) = (bool,map VBase (tuple key))


sameLength nm [] = error ("Function with no body: "++show nm)
sameLength nm [(ps,e)] = Just (length ps)
sameLength nm ((ps,e):more) = 
    if length ps == m
       then Just m
       else error ("Function "++show nm++" has different arities.")
  where (Just m) = sameLength nm more

primConstr typ nm args vs = VCon (length args) typ nm vs

apply f [] = f
apply (VFun f) (v:vs) = apply (f v) vs
apply f xs = error ("Non function\n  "++show f++"\nin apply to args\n  "++show xs)
     
----------------------------------
-- environments

newtype VEnv = VEnv (Int,[(Name,Integer,Value)])
valuesOf (VEnv (_,vs)) = vs

instance Show VEnv where
  show env = showEnv 3 env

showEnv n (VEnv (nextpropvar,zs)) = (plistf id "(" (map f (take n zs)) "," ")")
    where f (Nm (nm,p),uniq,val) = nm++"="++show val


evalMVar:: Name -> VEnv -> (Integer,Value)
evalMVar nm (VEnv (_,[])) = error (near nm++"Variable not found: "++show (name nm))
evalMVar nm (VEnv(n,(name,uniq,v):more)) =
  if nm==name then (uniq,v) else evalMVar nm (VEnv(n,more))

extendEnvName vs (VEnv (n,xs)) = VEnv (n,vs ++ xs)

-- This could be more complete by running computations to find structure to match against.
matchM:: VEnv -> Pat -> Value -> IO(Maybe VEnv)
matchM (VEnv (n,vs)) p v = 
     case (match [] p v) of
       Just pairs -> do { us <- mapM new pairs; return(Just(VEnv(n,us++vs)))}
       Nothing -> return Nothing
  where new (nm,v) = do { u <- nextinteger; return(nm,u,v)}
  
matchML env [] [] = return(Just env)
matchML env (p:ps) (v:vs) =
  maybeM (matchM env p v)
         (\ env2 -> matchML env2 ps vs)
         (return Nothing)


-- env1:: ENV
env1 = zipWith locate env0 [1..]
  where locate (x,v) n = (Nm(x,prelude),n,v)
firstNewInteger = getuniq(last env1) + 1

evenI :: Int -> Bool
evenI x = even x

traceM = VFunM 99 (\ mess k -> (k(VFunM 100 (\ v k2 -> putStrLn ("[Trace "++show mess++" "++show v++"]")>> k2 v))))

env0 =  [("+",liftIII3 "+" (+) (:+:) (+) plusM)
        ,("-",liftIII3 "-" (-) (:-:) (+) minusM)
        ,("*",liftIII3 "*" (*) (:*:) (+) timesM)
        ,("/",liftIII3 "div" (div) (DIV) (/)(unsup "div") )
        ,("div",liftIII3 "div" (div) (DIV) (unsup "div") (unsup "div"))        
        ,("mod",liftIII3 "mod" (mod) (MOD) (unsup "div") (unsup "mod"))        
       
        ,("<",liftIIB3 "<" (<) (:<) (ltM)(<) )
        ,("<=",liftIIB3 "<="  (<=) (:<=)(lteqM) (<=) )
        ,(">=",liftIIB3 ">=" (>=) (:>=) (gteqM) (>=))
        ,(">",liftIIB3 ">" (>) (:>) (gtM) (>))
        ,("==",liftIIB3 "==" (==) (:=) (eqM) (==))
        ,("/=",liftIIB3 "/=" (/=) (:/=) (unsup "/=") (/=))
        
        ,("&&",andV)
        ,("<=>",equalV)
        ,("||",orV)
        ,("not",notV)
        ,("=>",impliesV)
        
        ,("pnG",pnGV)
        ,("pnG2",pnG2V)        
        ,("show",showV)
        ,("setToList",tolistV)
        ,("setToArray",setToArrayV)
        ,("arrayDim",arrayDimV)
        ,("setDim",setDimV)
        ,("index",indexV)
        ,(".",indexV)
        ,("array",arrayV)
        ,("enum",enumV)
        ,("elem",elemV)
        ,("trace",traceM)
        ,("even",to evenI)
        
        ,("union",unionV)
        ,("complement",complementV)
        ,("intersect",intersectV)
        ,("difference",differenceV)
        
        ,("select",selectV)
        ,("project",projectV)
        ,("join",joinV)
        
        ,("full",fullV)
        ,("some",someV)
        ,("none",noneV)
        ,("one",oneV) 
        ,("emptyRel",emptyRelV)
        ,("fullRel",fullRelV)
        ,("subset",subsetV)
       
        ,("parent" , VSet BoolI parent)   
        ,("person" , VSet BoolI person)
        ,("v1", to (array (1,4::Int) [(1,5::Int),(2,55),(3,0),(4,8)]))
        ,("v2", to (array (1,6::Int) [(1,"Tim"),(2,"Mary"),(3,"Tom"),(4,"abc"),(5,"Frank"),(6,"John")]))
        ,("m1", to (array ((1,0::Int),(2,4::Int)) [((i,j),i+j) | i <- [1..(2::Int)],j <-[0..4]]))
        ,("i4" , VDomain [dim 4])
        ,("pd" , VDomain [pd])
        
        ,(":",primConstr "List" ":" [Nothing,Just "List"] [])
        ,("[]",primConstr "List" "[]" [] [])  

        ,("Just",primConstr "Maybe" "Just" [Nothing] [])  
        ,("Nothing",primConstr "Maybe" "Nothing" [] [])    
   
        ,("L",primConstr "Either" "L" [Nothing] [])  
        ,("R",primConstr "Either" "R" [Nothing] [])  
  
        ,("True",primConstr "Bool" "True" [] [])   
        ,("False",primConstr "Bool" "False" [] [])      
   
   ]
  
venv0 = VEnv (5,[])


type ENV = [(Name,Value)] 

nextPropVar (VEnv (n,vs)) = n
setPropVar n (VEnv(_,vs)) = VEnv(n,vs)

evalVar:: String -> Name -> ENV -> Value
evalVar x nm xs = find xs -- (valuesOf xs)
  where find [] = error ("No such "++x++" name: "++name nm ++ "\n" ++
                         (plistf id "(" (map (name . fst) xs) "," ")"))
        find ((a,c):_) | nm==a = c
        find (x:xs) = find xs
    

extend env more = more ++ env
  
emptyRelV = VFun h
  where h (VDomain ds) = VSet BoolI (emptyRel ds)
        h v = error ("emptyRel called with non dimension:\n"++show v)

fullRelV = VFun h
  where h (VDomain ds) = VSet BoolI (fullRel ds)
        h v = error ("fullRel called with non dimension:\n"++show v)
  
                                                        
----------------------------------------------

mkFun:: Integer -> (forall a. Value -> (Value -> IO a) -> IO a) -> (Value -> IO b) -> IO b
mkFun n f k = k(VFunM n f)

thunk:: IO v -> IO (IO v)
thunk x = 
  do { ref <- newIORef (Left x)
     ; let get = do { x <- readIORef ref; overwrite x}
           overwrite (Right x) = return x
           overwrite (Left c) =  
             do { x <- c      
                ; writeIORef ref (Right x)            
                ; return x }
     ; return get }
   
applyV :: Expr -> Expr -> Value -> Value -> (Value -> IO b) -> IO b
applyV g y fun arg k = 
        -- putStrLn("\napply: ("++show (EApp g [y])++"). "++show fun++" "++show arg) >>
        help fun arg k where
   help (VCon arity ty c vs) v k | arity > length vs = k(VCon arity ty c (vs ++ [v]))
   help (VFunM n vf) x k =  vf x k
   help (VFun g) x k = k(g x)   
-- help f (VCode fun) v k = do { arg <- reify i v; k(VCode(App fun arg))}
   help nonfun x k = error ( {- near f ++ -}"A nonfunction ("++show nonfun++") in function position.\nArising from the term\n "++show(EApp g [y]))

--------------------------------------------------------------------------

truePattern (PCon (Nm("True",_)) []) = True
truePattern (PLit pos (LCon "Bool" "True")) = True
truePattern _ = False

falsePattern (PCon (Nm("False",_)) []) = True
falsePattern (PLit pos (LCon "Bool" "False")) = True
falsePattern _ = False

{-
trymatching env ms [] v k = 
    case (ms) of
      ([(p1,e1),(p2,e2)]) 
         |  truePattern p1 && falsePattern p2 
         -> case v of
              VNS (NSYices tst) -> reifyC e1 env (\ x -> reifyC e2 env (\ y -> k(VNS(NSYices(IF tst x y)))))
              other -> error ("If on "++splat other)
      ([(p1,e1),(p2,e2)]) 
         | falsePattern p1 && truePattern p2
         -> case v of
              VNS (NSYices tst) -> reifyC e2 env (\ x -> reifyC e1 env (\ y -> k(VNS(NSYices(IF tst x y)))))         
              other -> error ("If on "++splat other)                  
      other -> error ("No clause matches scrutinee\ncase "++show v++" of"++displayCls ms)                   
-}      

displayCls ms = plistf g "\n  " ms "\n  " "\n"
  where g (p,e) = show p++ " -> "++show e

-------------------------------------------------------------
-- Reification under SMT

reifyC:: Expr -> VEnv -> (ExpY -> IO a) -> IO a
reifyC e env k = evalC e env (k . reify)


--------------------------------------------------
-- The evaluator

evalC:: Expr -> VEnv -> (Value -> IO a) -> IO a
evalC exp env k =   -- println ("Entering eval: "++ show exp) >> 
                    heval exp env k where 
  heval:: Expr -> VEnv -> (Value -> IO a) -> IO a                    
  heval (ELit loc x) env k = k(VBase x)
  heval (EVar nm) env k = k v where (uniq,v) = evalMVar nm env 
  heval (EApp f [x]) env k = 
     evalC f env (\ v1 -> evalC x env (\ v2 -> applyV f x v1 v2 k))
  heval (EApp f xs) env k = evalC (toBinary f xs) env k 
  heval (EAbs ms) env k = 
    do { i <- nextinteger
       ; let tryMatching:: VEnv -> [(Pat, Expr)] -> Value -> (Value -> IO a) -> IO a
             tryMatching env [] v k = 
                error ("No clause in\n   "++displayCls ms++
                       "matches the argument\n   "++show v)
             tryMatching env ((p,e):more) v k = 
               do { m <- matchM env p v
                  ; maybe (tryMatching env more v k)
                          (\ env2 -> evalC e env2 k) m }
       ; mkFun i (tryMatching env ms) k }
  heval (ELet d e) env k = evalDecC env d (\ env2 -> evalC e env2 k)                
  heval (ETuple TUPLE xs) env k = evalList [] xs env k
    where evalList vs [] env k = k(VTuple (reverse vs))
          evalList vs (x:xs) env k = 
             evalC x env (\ v -> evalList (v:vs) xs env k)
  heval (ETuple DIM xs) env k = evalList [] xs env k
    where evalList vs [] env k = k(VDomain (concat(reverse vs)))
          evalList vs (x:xs) env k = evalC x env cont
            where cont (VDomain v) = evalList (v:vs) xs env k
                  cont v = error (near exp ++"Non dimension in multi-width dimension:\n"++show v)           
  heval (ESet doms body) env k = evalC doms env cont1
     where cont1 (VDomain ds) = evalC body env (createSet2 k ds)
           cont1 v = error (near doms++"First arg to 'set' is not a dimension:\n"++show v)
  heval (term@(EComp (Range i j))) env k = 
    evalC i env (\ v1 -> evalC j env (\ v2 -> 
       case (v1,v2) of
         (VBase(LInt lo),VBase(LInt hi)) -> k (listVal(map (VBase . LInt) [lo .. hi]))
         (lo,hi) -> error ("Range expression\n   "++show term++"\ndoes not evaluate to int pair:\n   "++show (lo,hi))  ))  
  heval (EComp (Comp body xs)) env k = 
       do { envs <- (foldM compD2 [env] xs)  
          ; vs <- evalOverMany2 body envs
          ; k(to vs)
          }
  heval (EForm (Left form)) env k =
    do { (sh,v) <- evalForm env form; k v}
  heval (EForm (Right c)) env k =
    do { v <- evalCon c env; k v}    
    
--------------------------------------------
-- Evaluate a constraint to a Boolean Value

evalCon:: Constraint -> VEnv -> IO Value 
evalCon (None form) env = 
  do { (sh,v) <- evalForm env form
     ; noneVal v }
evalCon (One form) env = 
  do { (sh,v) <- evalForm env form
     ; oneVal v }
evalCon (Some form) env = 
  do { (sh,v) <- evalForm env form
     ; someVal v }    
evalCon (Full form) env = 
  do { (sh,v) <- evalForm env form
     ; fullVal v }   
evalCon (Subset x y) env = 
  do { (sh,xv) <- evalForm env x
     ; (sh,yv) <- evalForm env y
     ; subsetVal xv yv }
evalCon (d@(FunDep form doms rngs)) env = 
  do { (sh,xv) <- evalForm env form
     ; let args = map fst sh
           dom = map (getIndex (show d) args) doms
           rng = map (getIndex (show d) args) rngs
     ; case xv of
        VSet BoolI set -> do { prop <- funDep dom rng set; return(to prop)}
        VSet PropI set -> do { prop <- funDep dom rng set; return(to prop)}
        VSet BddI set  -> do { prop <- funDep dom rng set; return(to prop)}
        v -> error ("Functional dependency is not a set: "++show v)
     }
evalCon (RightArrow x y) env = 
  do { (sh1,xv) <- evalForm env x
     ; (sh2,yv) <- evalForm env y
     ; let (sh,left,right) = disjShape 0 sh1 sh2
     ; xset <- projectVal left xv
     ; yset <- projectVal right yv
     ; subsetVal xset yset }
       
evalCon (Fact form) env = 
  do { (sh,v) <- evalForm env form
     ; someVal v } 
evalCon (BoolC "&&" xs) env = 
  do { pairs <- mapM (\ x -> evalCon x env) xs
     ; foldrM andVal (to True) pairs }
evalCon (BoolC "||" xs) env = 
  do { pairs <- mapM (\ x -> evalCon x env) xs
     ; foldrM orVal (to False) pairs }     
evalCon (BoolC "not" [x]) env = 
  do { v <- evalCon x env
     ; notVal v }
evalCon (BoolC "<=>" [x,y]) env = 
  do { v1 <- evalCon x env
     ; v2 <- evalCon y env
     ; equalVal v1 v2 }
evalCon (BoolC str xs) env = error ("Unrecognized boolean combination of constraints: "++str)
evalCon (All args range con) env = error ("No All con yet")
{-
  do {(shape,VSet tag (FA ds map)) <- evalForm env (Embed args range)
     ; case tag of
         PropI -> processAll env args ds (DM.toList map) con
         BoolI -> processAll env args ds 
                    (fmap (\ (x,y) -> (x,toProp y)) (DM.toList map))
                    con
     }

processAll :: VEnv -> [String] -> [Dimension] -> [(Key,Prop Int)] -> Constraint -> IO Value
processAll env args dims pairs con = 
  do { let f (Key _ lits,b) = 
             do { 
               -- ; putStrLn "\n-------------------------------------"
               -- ; putStrLn(show lits ++"="++ show b);
                  let con2 = subCon (zip args lits) con
               -- ; putStrLn(show con2)
                ; val <- evalCon con2 env
                ; let www = (andOpt b (toPropInt val))
               -- ; putStrLn(show www)
                ; return www
                }
     ; ps <- mapM f pairs
     ; return (VNS (NSProp(andL ps)))
     }
-}


getIndex mess xs s =
  case elemIndex s xs of
    Nothing -> error ("The var: "++show s++" is not amongst the args "++show xs++"\nIn functional dependency\n"++mess)
    Just n -> n

-----------------------------------------------------------
-- helper functions

-- Create a set from a list of dimensions and a value that 
-- encodes a list of elements 

createSet2 :: (Value -> IO a) -> [Dimension] -> Value -> IO a
createSet2 k dims v 
  | Just lits <- isListV v
  = do { -- putStrLn("Lits = "++show lits);
         -- putStrLn("Dims = "++show dims);
         -- putStrLn("Restrict = "++show((restrict dims lits)::FiniteSet Bool));
         k(VSet BoolI (manyD dims lits)) }
createSet2 k ds v = error("Bad inputs to set\n   "++show ds++"\n   "++show v) 

----------------------------------------------------------
-- for recursion

clausef nm (ps,e) = plistf show (show nm++" ") ps " " " = " ++ show e
badClauseMatch nm vs cls = 
  "No clause matches "++plistf show "" vs " " "\n"++ (plistf (clausef nm) "\n" cls "\n" "\n")

extRecFunC:: Int -> Name -> [([Pat], Expr)] -> VEnv -> IO VEnv       
extRecFunC n nm cls env = 
   do { uniq <- nextinteger
      -- ; putStrLn("REC FUN "++show nm++" has unique "++show uniq)
      ; let unwind:: VEnv -> Int -> [Value] -> (Value -> IO b) -> IO b
            unwind env 0 vs k = matchTreeClauseM3 nm cls env (reverse vs) cls k
            unwind env n vs k = 
                do { i <- nextinteger
                   ; k (VFunM i (\ v k1 -> unwind env (n-1) (v:vs) k1)) }       
            recEnv env2 = do { vfun <- unwind env2 n [] return
                             ; return((extendEnvName [(nm,uniq,vfun)] env))}
      ; fixIO recEnv }



matchTreeClauseM3:: Name -> [([Pat], Expr)] -> VEnv -> 
                      [Value] -> [([Pat], Expr)] -> 
                      (Value -> IO b) -> IO b  
matchTreeClauseM3 nm all env vs cls k = tryMatching env cls vs k
   where tryMatching:: VEnv -> [([Pat], Expr)] -> [Value] -> (Value -> IO a) -> IO a
         tryMatching env [] vs k = 
             error ("No clause in\n   "++displayCls all++
                    "from the recursive function "++show nm++"\n"++
                    "matches the arguments\n   "++show vs++show (map showVal vs))
         tryMatching env ((ps,e):more) vs k = 
               do { m <- matchML env ps vs
                  ; maybe (tryMatching env more vs k)
                          (\ env2 -> evalC e env2 k) m }

-- for comprehensions

-- toList e (VDomain [Dim n base ls]) = map VBase ls
toList e (VDomain ds) = map (f . map VBase) (allTuples ds)
  where f [x] = x
        f xs = VTuple xs
toList e v | Just vs <- isListV v =  vs
toList e v = error ("In comprehension  term:\n   "++show e++"\nthe value is not list like\n   "++show v)

dA = Dim 2 Int [LInt 1,LInt 2]
dB = Dim 3 String [LString "A",LString "B",LString "C"]

 
compEnvC2 :: CompTerm Expr Expr -> VEnv -> IO[VEnv]
compEnvC2 (Pred p) env = evalC p env (\ v -> if valTrue v then return [env] else return [])
compEnvC2 (Generator p e) env =  evalC e env (cont . toList e)
  where cont [] = return []
        cont (v:vs) = 
          do { u <- (matchM env p v); us <- cont vs; return(add u us)}
        add Nothing xs = xs
        add (Just x) xs = x:xs  
 
compD2 :: [VEnv] -> CompTerm Expr Expr ->  IO[VEnv]
compD2 [] c = return []
compD2 (env:envs) c = 
  do { xs <- compEnvC2 c env
     ; ys <- compD2 envs c
     ; return(xs++ys)}

evalOverMany2 :: Expr -> [VEnv] -> IO[Value]     
evalOverMany2 body [] = return []
evalOverMany2 body (e:es) = 
  do { v <- evalC body e return
     ; vs <- evalOverMany2 body es
     ; return(v : vs)} 
     
-------------------------------------------------------------------
-- evaluating declarations extends the environment

evalDecC :: VEnv -> Decl -> (VEnv -> IO a) -> IO a 
evalDecC env (Rule2 pos nm (DeclRhs xs)) k = 
  error ("Rule "++nm++" has a declaration but no definition.")
evalDecC env (Rule2 pos nm (DefRhs _ _)) k = 
  error ("Rule "++nm++" has a definition but no declaration.")  
evalDecC env (d@(Rule2 pos nm (Both args form))) k =  
  do { let cont nm (VDomain [d]) = return (nm,d)
           cont nm v = error ("Non (unit-width) dimension type in rule: "++show v)
           strip (VEnv (n,xs)) = (map (\ (nm,uniq,v) -> (nm,v)) xs)
           name = Nm(nm,pos)
     ; shape <- mapM (\ (nm,e) -> evalC e env (cont nm)) args
     ; uniq <- nextinteger
     ; env2 @(VEnv(next,(nm,u,v):_)) <- fixPoint2 1 shape d ( name,uniq,VSet BoolI (emptyRel (map snd shape))) env
     ; k env2 -- k(extendEnvName [(nm,uniq,v)] env) 
     }
evalDecC env (d@(Def _ pat exp)) k | any (boundby pat) (freeExp exp []) = 
     error ("Recursive definition\n"++show d++"\nUse clausal form to capture recursion.")
  where boundby pat x = elem x bound where (free,bound) = (patBinds pat ([],[]))

evalDecC env (d@(Def loc pat exp)) k = 
   evalC exp env (\ v -> maybeM (matchM env pat v) k 
                                (error (near loc++"\nThe pattern\n   "++show pat++"\nCan't match the value\n   "++show v)))

evalDecC env (DataDec pos t cs) k = 
      do { i <- nextinteger; env2 <- (foldM acc (enum i) cs); k(extendEnvName env2 env) }
  where nullary (c,args) = null args
        new (c,[]) = LCon t c
        enum i = if (all nullary cs)
                  then [(Nm(t,pos),i,VDomain[Dim (length cs) (Enum t) (map new cs)])]
                  else []
        
                 
        acc env (c,args) = 
            do { i <- nextinteger
               -- ; return ((Nm(c,pos),i,primConstr t c args []):env)}
               ; case args of
                  [] -> -- A nullary Constr is a Literal
                        return((Nm(c,pos),i,VBase(LCon t c)):env)
                  _ -> return((Nm(c,pos),i,VCon (length args) t c []):env)}
                              
evalDecC env (FunDec pos nm cls) k | Just n <- sameLength nm cls = 
  do { env2 <- extRecFunC n (Nm(nm,pos)) cls env; k env2 }
                
evalDecC env (d@(Domain pos typ nm exp)) k = evalC exp env cont
  where cont v | Just vs <- isListV v = 
                 do { uniq <- nextinteger
                    ; let toDim vs = VDomain[Dim (length vs) (litToBase (unLit(head vs))) (map unLit vs)]
                    ; k(extendEnvName  [(Nm(nm,pos),uniq,toDim vs)] env) }
        cont v = error ("Non list in dimenson declaration:\n   "++show v++"\n"++show d)    

evalDecC env (d@(Find pos vs constraint strategy tech)) k = 
  do { (deltaEnv,ts,ss) <- foldrM (initNS tech) (env,[],[]) vs
     ; putStrLn("Elaborating constraint.")
     ; delta <- evalC constraint deltaEnv (solveCon env deltaEnv ts ss strategy tech)
     ; env2 <- bind delta env
     ; k env2 }

-------------------------------------------------------------------
-- Generate a bunch of names to extend the environment with 
-- from the initalizing clauses. Some names will be invented names

initNS:: Technique -> (Pat,Initializer Expr Expr) -> 
         (VEnv,[(Name,Value)],[(Name,TypY,Value)]) -> 
         IO (VEnv,[(Name,Value)],[(Name,TypY,Value)])
initNS tech (PTuple TUPLE ps,Ituple xs) (env,ts,ss) 
   | length ps == length xs = foldrM (initNS tech) (env,ts,ss) (zip ps xs)

initNS SAT (PVar nm,i1@(Iset i low high)) (env,ts,ss) = 
  do { i2@(Iset ds lowv highv) <- evalInit env i1
     ; lowSet <- isSet low lowv
     ; highSet <- isSet high highv
     ; let n = nextPropVar env
           (m,set) = enumByKey (countWithBounds (lowSet,highSet)) ds n
     ; env3 <- bind [(nm,VSet PropI set)] (setPropVar m env)
     ; let set2 = VSet PropI set
     ; return(env3,(nm,set2):ts,(nm,yicesType i2,set2):ss)}

initNS SMT (PVar nm,i) (env,ts,ss) = 
  do { i2 <- evalInit env i
     ; let (zs,v,m) = smtEta nm (nextPropVar env) i2
     ; env2 <- bind [(nm,v)] (setPropVar m env)
     ; return(env2,(nm,v):ts,zs++ss)}
initNS IP (PVar nm,i) (env,ts,ss) =
  do { i2 <- evalInit env i
     ; let (vs,v,morenames) = ipEta (loc nm) names i2
     ; env2 <- bind [(nm,v)] env
     ; return(env2,(nm,v):ts,(nm,yicesType i2,v):ss)}
initNS tech (pat,i) (env,ts,ss) = 
   error (near pat++"\nDon't know how to initialize\n   "++show pat++
          "\nwith the initializer\n   "++show i++
          "\nusing the solver "++show tech)
          
-------------------------------------------------------------------



solveCon:: VEnv -> VEnv -> 
           [(Name,Value)] ->        -- Names in the Initialization, end up in final env
           [(Name,TypY,Value)] ->   -- Extra Names to Declare to SMT
           Strategy -> Technique -> Value -> 
           IO [(Name, Value)]

solveCon env deltaEnv ts ss Abstract tech v = 
  do { let f (n,v) = show n ++" = "++show v
     ; putStrLn("\n-------------------------------------------")
     ; putStrLn("\nAbstract values"++plistf f "\n  " ts "\n  " "")
     ; let (depth,size,lits) = propStat (toPropInt v)
     ; putStrLn("\nAbstract where clause\n   depth = "++
               show depth++"\n   size = "++show size++"\n   vars = "++show lits)
     ; return ts }
solveCon env (deltaEnv@(VEnv(_,(nm,_,_):_))) ts ss kind IP (VNS (NSRel xs)) = 
  do { let (e,mode) = case kind of
                       (Min e) -> (e,"-minimize")
                       (Max e) -> (e,"-maximize")
     ; VNS(NSMath w) <- evalC e deltaEnv return
     ; let (obj,cs,(col,row,ss)) = fillProb w xs
     --; putStrLn(plistf show "" cs "\n" ("\n"++show obj))
     ; putStrLn(boxWithLabels row col ss)
     ; mpsPrint (show nm) w col xs
     ; putStrLn(plistf shRng ("maximize "++show w++" where\n") xs "\n" "\n")
     ; let mpsFile = ("tmp/"++show nm++".mps")
           solFile = ("tmp/"++show nm++".sol") 
     ; ans <- solveMPS mode mpsFile solFile (show nm) w col xs 
     ; let f (s,i) = (s,VBase(LInt i))
           subst = (DM.fromList(map f ans))
           subf (nm,v) = (nm,subVal subst v)
     ; putStrLn("ANSWERS "++show ans)
     ; putStrLn(show(map subf ts))
     ; return(map subf ts)
     }
solveCon env deltaEnv ts ss strat SAT v = 
  do { let vars = varsV v 
           prop = propFromValue v
           f sol (nm,VSet PropI set) = (nm,VSet BoolI (instan sol set))
           toEnv sol = return(map (f sol) ts)
           nm [] = "sat"
           nm ((nm,v):_) = name nm
           cnfFile = ("tmp/"++nm ts++".cnf")
           solFile = ("tmp/"++nm ts++".sol") 
           (depth,size,lits) = propStat (toPropInt v)
           gg :: Expr -> [Int] -> IO(Value,[(Name, Value)])
           gg xxx sol = do { let delta = map (f sol) ts
                           ; putStrLn("SOL "++show sol)
                           ; env2 <- bind delta env
                           ; evalC xxx env2 (\ v -> return(v,delta)) }
     ; putStrLn("Where clause information\n   depth = "++
                show depth++"\n   size = "++show size++"\n   vars = "++show lits)
     ; (next,cnfForm) <- toCnf prop    -- cnfForm = cnf2 prop
     ; case strat of
         First -> firstMinisat toEnv cnfFile solFile cnfForm
         (Many xxx) -> chooseMinisat (gg xxx) cnfFile solFile cnfForm
         strat -> error ("Can't use this strategy with SAT ")
     }
solveCon env deltaEnv ts ss First SMT (v@(VNS (NSYices e))) = 
         do { let f (n,v) = show n ++" = "++show v
            ; putStrLn("\n-------------------------------------------")
            ; putStrLn("\nAbstract values"++plistf f "\n  " ts "\n  " "")
            ; putStrLn("\nAbstract where clause\n"++show v)
            ; putStrLn("YICES INPUT\n"++unlines (map show cmds))
            ; sol <- runYices ["-e"] cmds
            ; interp sol }
   where cmds = map define ss ++ [ASSERT e]
         define (nm,i,v) = DEFINE (name nm,i) Nothing
         interp (Sat sol) =  putStrLn ("\nYICES output\n"++show sol) >>
                             return(map (\ (nm,v) -> (nm,subVal subst v)) ts)
            where subst = (DM.fromList (map oneExp sol))
         interp (UnSat s) = error ("SMT fails\n"++s)
         interp (Unknown s) = error ("SMT fails\n"++s)
         interp (InCon s) = error ("SMT fails\n"++unlines s)         

------------------------------------------------
-- eta-Expand an Initializer into an SMT value

smtEta:: Name -> Int -> Initializer [Dimension] Value -> ([(Name,TypY,Value)],Value,Int)
smtEta nm n (Ilist size i) = (xs,listVal vs,m) 
  where (xs,vs,m) = smtEtaL nm n (take size (repeat i))
smtEta nm n (Ituple is) = (xs,VTuple vs,m)
  where (xs,vs,m) = smtEtaL nm n is
smtEta nm n (Iarray [d] i) = (xs,VVector d (listArray (0,count - 1) vs),m)
  where count = dimSize d
        (xs,vs,m) = smtEtaL nm n (take count (repeat i))
smtEta nm n (Iarray [d1,d2] i) = (xs,VMatrix d1 d2 (listArray ((0,0),(size1 -1,size2 -1)) vs),m)
  where size1 = dimSize d1
        size2 = dimSize d2
        (xs,vs,m) = smtEtaL nm n (take (size1 * size2) (repeat i))
smtEta nm n (Iarray ds i) = error (near nm++"\nUnsupported dimension in Array intitializer\n"++show ds++"\nCurrently we support only 1 and 2 dimensional arrays.")
smtEta nm n (i@(Iset _ _ _)) = error (near nm++"\nSet in SMT intitializer\n"++showInitializer i)
-- all base types are treated the same
smtEta (Nm(old,pos)) n i = ([(name,yicesType i,v)],v,n+1) 
  where name = Nm(new,pos)
        new = old++show n
        v = VNS(NSYices(VarE new))

smtEtaL nm n [] = ([],[],n)
smtEtaL nm n (i:is) = (xs++ys,v:vs,n3)
    where (xs,v,n2) = smtEta nm n i
          (ys,vs,n3) = smtEtaL nm n2 is

-- an infinite list of Strings
names = (map (:[]) "abcdefghijklmnopqrstuvwxyz") ++ (map ('z':) names)

ipEta:: SourcePos -> [String] -> Initializer [Dimension] Value -> ([(Name,Value)],Value,[String])
ipEta pos ns (Ilist size i) = (xs,listVal vs,m) 
  where (xs,vs,m) = ipEtaL pos ns (take size (repeat i))
ipEta pos names (Ituple is) = (xs,VTuple vs,m)
  where (xs,vs,m) = ipEtaL pos names is  
ipEta pos names (Iarray [d] i) = (xs,VVector d (listArray (0,count - 1) vs),m)
  where count = dimSize d
        (xs,vs,m) = ipEtaL pos names (take count (repeat i))
ipEta pos names (Iarray [d1,d2] i) = (xs,VMatrix d1 d2 (listArray ((0,0),(size1 -1,size2 -1)) vs),m)
  where size1 = dimSize d1
        size2 = dimSize d2
        (xs,vs,m) = ipEtaL pos names (take (size1 * size2) (repeat i))
ipEta pos names (Iarray ds i) = error (near pos++"\nUnsupported dimension in Array intitializer\n"++show ds++"\nCurrently we support only 1 and 2 dimensional arrays.")
ipEta pos (n:ns) Iint = ([(name,v)],v,ns)
   where name = Nm(n,pos)
         v = VNS(NSMath (Term [(n,1)] 0))
ipEta pos names i = error (near pos++"\nUnsupported type in IP intitializer\n"++showInitializer i)
        
         
ipEtaL pos names [] = ([],[],names)
ipEtaL pos names (i:is) = (xs++ys,v:vs,n3)
    where (xs,v,names2) = ipEta pos names i
          (ys,vs,n3) = ipEtaL pos names2 is        
   
   
----------------------------------------
-- evaluating Initializations

isDomain i (VDomain ds) = ds
isDomain i v = error (near i++"\nIn exist problem, domain evaluates to non-dimension:\n  "++show v)

isSet e (VSet BoolI (FA ds y)) = return y
isSet e v = error(near e++"\nTerm:\n    "++show e++"\nEvaluates to non concrete set:\n   "++show v)

bind xs env = 
  do { let f (nm,v) = do { uniq <- nextinteger; return(nm,uniq,v)}
     ; ys <- mapM f xs
     ; return(extendEnvName ys env) }

evalInit :: VEnv -> Initializer Expr Expr -> IO (Initializer [Dimension] Value)
evalInit env Iint = return Iint
evalInit env Ibool = return Ibool
evalInit env Idouble = return Idouble
evalInit env (Idomain e) = evalC e env (return . Idomain . isDomain e)
evalInit env (Iarray e i) = 
  do { ds <- evalC e env (return . isDomain e)
     ; i <- evalInit env i
     ; return(Iarray ds i)}
evalInit env (Ilist n i) = 
  do { i <- evalInit env i
     ; return(Ilist n i)}     
evalInit env (Ituple xs) = fmap Ituple(mapM (evalInit env) xs)
evalInit env (Iset i x y) =
  do { ds <- evalC i env (return . isDomain i)
     ; let pos = loc i
     ; env2 <- bind [(Nm("none",pos),VSet BoolI (emptyRel ds))
                    ,(Nm("full",pos),VSet BoolI (fullRel ds))] env                    
     ; x2 <- (evalC x env2 return)
     ; y2 <- (evalC y env2 return)
     ; return(Iset ds x2 y2)}

yicesType:: Initializer [Dimension] Value -> TypY
yicesType x =
  case x of
    Iint -> VarT "int"
    Ibool -> VarT "bool"
    Idouble -> VarT "real"
    Ituple xs -> TUP(map yicesType xs)
    Idomain [d] -> dimToYices d
    Idomain ds -> TUP(map dimToYices ds)
    Iset _ _ _ -> error ("Sets cannot be reasoned about using SMT\n"++showInitializer x)
      
      
showInitializer x = render(ppInit (\ x -> text(plistf dimName "(" x "," ")")) (text . show) x) 
  
     

dimToYices (Dim n Int ls) = SUBTYPE("x",VarT "int")(OR(map (eqf (VarE "x")) ls))
dimToYices (Dim n Bool ls) = SUBTYPE("x",VarT "bool")(OR(map (eqf (VarE "x")) ls))
dimToYices (Dim n Double ls) = SUBTYPE("x",VarT "real")(OR(map (eqf (VarE "x")) ls))
dimToYices d = error ("Only dimensions over base types can be used in SMT\n"++dimNameAll d)

         
eqf x lit = (x := (reify (VBase lit)))
                     
--------------------------------------------------------------
-- Creating and using substitutions with Yices

-- Yices returns [ExpY] like: [(= y2 true),(= z5 0),(= y1 1)]
-- we need to turn that into [(String,Value)] 

toDouble:: Rational -> Double
toDouble y = (toD (numerator y)) / (toD (denominator y))
  where toD:: Integer -> Double
        toD x = fromInteger x

expYtoV:: ExpY -> Value
expYtoV (LitB b) = to b
expYtoV (LitI n) = to n
expYtoV (LitR r) = VBase(LDouble (toDouble r))
expYtoV (MKTUP xs) = VTuple(map expYtoV xs)
expYtoV x = error ("Can't turn SMT expression into a value\n  "++show x)

oneExp (VarE s := e) = (s,expYtoV e)
oneExp (e := VarE s) = (s,expYtoV e)
oneExp x = error("SMT solver returns non assignment:\n  "++show x)

subVal :: DM.Map String Value -> Value -> Value
subVal env (VTuple xs) = VTuple (map (subVal env) xs)
subVal env (VCon arity c ty xs) = VCon arity c ty (map (subVal env) xs)
subVal env (VVector d arr) = VVector d (fmap (subVal env) arr)
subVal env (VMatrix d1 d2 arr) = VMatrix d1 d2 (fmap (subVal env) arr)
subVal env (v@(VNS (NSYices (VarE x)))) =
  case DM.lookup x env of
    Just v2 -> v2
    Nothing -> v
subVal env (v@(VNS (NSMath(Term [(x,1)] 0)))) =
  case DM.lookup x env of
    Just v2 -> v2
    Nothing -> v    
subVal env v = v    

instanSMT sol v = subVal (DM.fromList (map oneExp sol)) v



--------------------------------------------------------------

evalP env (PVar s) = return(PVar s)
evalP env (PLit pos l) = return (PLit pos l)
evalP env (PTuple x ps) = 
   do { qs <- mapM (evalP env) ps; return(PTuple x qs)}
evalP env (PCon c ps) = 
   do { qs <- mapM (evalP env) ps; return(PCon c qs)}
evalP env (PWild p) = return(PWild p)
evalP env (PEsc e) = evalC e env (return . valueToPat e)

evalB :: VEnv -> Atom -> IO(Value, [Dimension], Pat)
evalB env atom =
  do { (v,p) <- evalAtomB env atom
     ; case v of
        VSet _ set -> return (v,dimFA set,p)
        -- A domain denotes the full set over that domain
        VDomain ds -> let set = fullRel ds 
                          v = VSet BoolI set
                      in return(v,ds,p)
        v -> error ("Non Relational value from atomic formula in evalForm: "++show v)
     }
  
evalAtomB env (Prim name pat) = return (v,pat)
  where (_,v) = evalMVar name env
evalAtomB env (Escape pos exp pat) = evalC exp env (\ v -> return(v,pat))

d1 = dim 10
d2 = dimS ["Red","Blue","Green","Yellow"]
fp = shapeToPred [(pv "x",d1),(pv "y",d2)]
  where pv x = PVar(toName x)
set1:: FiniteSet (Prop Int)
set1 = manyD [d1,d2] [(1::Int,"Red"),(2,"Red"),(3,"Blue")]

evalForm:: VEnv -> Formula -> IO (Shape,Value)
evalForm env (Atomic xxx) = 
   do { (VSet tag set,ds,pat) <- evalB env xxx
      -- ; println ("^^^^^^^^^^^^^^\nEvalForm "++show xxx++"\n   "++show set)
      ; pat2 <- evalP env pat
      ; let ps = patterns pat2
      ; whenM (length ps /= length ds) 
              (near xxx++"The atom "++ show xxx ++ " has the wrong number of patterns"++
               " (it should have "++show (length ds)++").")
      ; let sh1 = zip ps ds
            (sh2,proj) = projShape sh1
      ; setAns <- project proj (select (shapeToPred sh1) set)
      ; return (sh2,VSet tag setAns)}
evalForm env (t@(Conj x y)) = 
  do { (shX,vX) <- evalForm env x
     ; (shY,vY) <- evalForm env y
     ; let (w@(n,sh1,col1,sh2,col2)) = joinShape (loc t) shX shY
     ; uX <- projectVal col1 vX
     ; uY <- projectVal col2 vY
     ; let sh = sh1 ++ drop n sh2
     ; ans <- joinVal n uX uY
     -- ; (sh,ans) <- cross (shX,vX) (shY,vY)
     -- ; putStrLn("Product "++show t++"\n shape = "++show sh++"\n shX = "++show shX++" shY = "++show shY)
     ; return(sh,ans)
     }
evalForm env (Negation x) = 
  do { (sh,v) <- evalForm env x 
     ; case v of
         (VSet tag set) -> do { ans <- complement set; return (sh,VSet tag ans) }
     }
evalForm env (Embed (pairs) x) = 
  do { (sh1,VSet tag set) <- evalForm env x
     ; let (sh2,ps) = lhsProjShape2 sh1 pairs
     ; ans <- (project ps set)
     ; return(sh2,VSet tag ans) }
evalForm env (Disj x y) = 
  do { (shX,vX) <- evalForm env x
     ; (shY,vY) <- evalForm env y
     ; disjunct (shX,vX) (shY,vY)
     }
evalForm env (f@(Equality p1 n p2)) = evalC (EVar n) env cont
  where cont (VDomain [d]) = 
          do { let set:: FiniteSet Bool
                   set = equal2 [d,d] 
             ; pat1 <- evalP env p1
             ; pat2 <- evalP env p2     
             ; let sh1 = zip [pat1,pat2] [d,d]
                   (sh2,proj) = projShape sh1
             ; ans <- (project proj (select (shapeToPred sh1) set))
             ; return (sh2,VSet BoolI ans) }
        cont v = error (near n++"\nNon (unit-width) dimension in equality: "++show n++"\n"++show f)


-- Union with implicit projection
disjunct:: (Shape,Value) -> (Shape,Value) -> IO(Shape,Value) 
disjunct (shX,valX) (shY,valY) = 
  do { let (sh,left,right) = disjShape 0 shX shY
     ; x1 <- (projectVal left valX)
     ; x2 <- (projectVal right valY)
     ; x3 <- unionVal x1 x2
     ; return(sh,x3)}

cross:: (Shape,Value) -> (Shape,Value) -> IO(Shape,Value)
cross (shX,valX) (shY,valY) = apply2 f34 valX valY
  where f34:: forall b . (BooleanM IO b) => SetI b -> FiniteSet b -> FiniteSet b -> IO(Shape,Value)
        f34 tag (FA dsX setX) (FA dsY setY) =
            do { (shape,set3) <- (crossProd (shX,DM.toList setX) (shY,DM.toList setY)) >>= subsToAns 
               ; return(shape,toFLSet tag set3)}               
               
                                            
fixPoint2 :: Int -> Shape -> Decl -> (Name,Integer,Value) -> VEnv -> IO VEnv
fixPoint2 n lhsshape (rule@(Rule2 pos nm (Both args form))) (name,uniq,v1) env = 
 do { let env2 = (extendEnvName [(name,uniq,v1)] env)
    ; (shape,v2 @ (VSet tag set2)) <- evalForm env2 form
    ; let (finalShape,proj) = lhsProjShape shape lhsshape
          f (v,d) = (EVar (Nm(v,pos)))
          g (v,d) = Nm(v,pos)
    -- ; putStrLn("\n############# Evaluating "++show nm)
    ; set3 <- runExprs (map snd lhsshape) -- resulting shape
                       (map f lhsshape)   -- expressions
                       env2               -- env
                       (map g shape)      -- rhs names
                       set2               -- set to obtain values for the rhs names
    ; let v3 = VSet tag set3
--     ; println ("Computing fix point\n"++show rule)
    --; println (showFromEnv "x" env)
    --; println ("V1 = "++show v1)
    --; println ("V2 = "++show v2)
    --; println ("V3 = "++show v3)
    ; if (v1==v3)
         then return env2
         else fixPoint2 (n+1) lhsshape rule (Nm(nm,pos),uniq,v3) env
       -- we really need to check that "args" and "shape" match up
    }       

unBase e (VBase t) = return t
unBase e v = error ("Evaluation does not lead to a literal.\n  "++show e++"\n  "++show v)


runExprs:: (BooleanM IO a) => [Dimension] -> [Expr] -> VEnv -> [Name] -> FiniteSet a -> IO(FiniteSet a)
runExprs ds exprs env names set = 
  do { let vs = toValues set
           -- f(x,y+1) <- rhs
           -- rhs computes tuples (ui,vi) found in 'set'
           -- 'exprs' = [x,y+1]
           eval env e = evalC e env (unBase e)
           transform (prop,values) =                          -- for each tuple (ui,vi)
              do { env2 <- extendMany (zip names values) env  -- add (x,ui),(y,vi) to env
                 ; lits <- mapM (eval env2) exprs             -- evaluate (x,y+1) in this env
                 ; return (toKey "runExprs" ds lits,prop) }              -- then create a new key from the values.         
     -- ; putStrLn("\n vars = "++show names++" expressions = "++show exprs++"\n vs = "++show vs)
     ; us <- (mapM transform vs)
     ; ans <- (DM.fromListWithM disjM us)
     ; return(FA ds ans)
     }
        
extendMany pairs env =
  do { let f (nm,v) = do { uniq <- nextinteger; return(nm,uniq,v)}
     ; trips <- mapM f pairs
     ; return (extendEnvName trips env) }
     

-------------------------------------------------------------

run1:: IO Value
run1 = trye "set (i4,i4) [(i,j) | i <- i4, j <- [0..6]]"

tryf s = evalForm (VEnv(0,env1)) (parse2 formulaP s)   
trye s = evalC (parse2 expr s) (VEnv(0,env1)) return



