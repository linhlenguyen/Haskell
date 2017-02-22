{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, 
             PatternGuards, RelaxedPolyRec, GADTs #-}
module Syntax where

import MyMap (toList,fromList,fromListWith,fromListWithM)  
import qualified MyMap as DM

import Data.List(sortBy,groupBy,nub,find,elemIndex,(\\))
import qualified Data.List as List
import Data.Array

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($$),($+$),render)

import Text.ParserCombinators.Parsec.Expr(Assoc(..))
import Text.ParserCombinators.Parsec.Pos(SourcePos,newPos)
import Debug.Trace
import Monads(noPos)

import Auxfuns (plistf,foldrM)
import UniqInteger(nextinteger)
import Literal
import Boolean
import Prop
import Dimension
import FiniteSet

----------------------------------------------------------------

newtype Name = Nm (String,SourcePos)

data Atom = Prim Name Pat                -- f(x,y,z)
          | Escape SourcePos Expr Pat    -- $(row n z)(x,y,z)
         

type LhsAtom = (Name,[(String,Maybe Dimension)]) 
 
data Formula
   = Atomic Atom                      -- father(x,y)
   | Conj Formula Formula             -- father(x,y), male(x)
   | Disj Formula Formula             -- young(x) ;  old(x)
   | Negation Formula                 -- !father(x,y)
   | Embed [String] Formula           -- {(y,x) <- r(x), z(x,y,z)}   Like a projection
   | Equality Pat Name Pat

data TKind = TUPLE | DIM deriving Eq
data NKind = Narrow | Choose

data Expr
   = ELit SourcePos Literal -- 5, 2.3, "abc", etc.
   | EVar Name              -- x , ?x1
   | EApp Expr [Expr]       -- e1 [e2,e3]
                            -- e1 * e2  === mult[e1,e3] (add,subtract,div)
   | EAbs [(Pat,Expr)]      -- (\ x -> f[x]+1)                           
   | ELet Decl Expr
   | ETuple TKind [Expr]    -- (2,3),  #(int,names)
   | ESet Expr Expr         -- set #(dom,width) [(2,"a")]
   | EComp Comprehension
   | EForm(Either Formula Constraint)

data Comprehension
  = Range Expr Expr
  | Comp Expr [CompTerm Expr Expr]
   
data CompTerm a b
  = Generator Pat a
  | Pred b
  
data Pat
   = PVar Name
   | PLit SourcePos Literal
   | PTuple TKind [Pat]
   | PCon Name [Pat]
   | PEsc Expr         -- for patterns inside formula only!
   | PWild SourcePos
   
data RuleRhs 
   = DeclRhs [(String,Expr)]         -- p(x,y) -> g(x), q(y).
   | DefRhs [String] Formula         -- p(a,y) <- g(a), h(a,y).
   | Both [(String,Expr)] Formula    -- the idea is that the two forms
                                     -- are parsed and then merged into a Both

data Decl 
   = Rule2 SourcePos String RuleRhs
   | Def SourcePos Pat Expr
   | DataDec SourcePos String [(String,[Maybe String])]
   | FunDec SourcePos String [([Pat],Expr)]
   | Domain SourcePos Base String Expr -- Expr should evaluate to a list of Base objects
   | Find SourcePos [(Pat,Initializer Expr Expr)] Expr Strategy Technique

data Strategy = First | Many Expr | Max Expr | Min Expr | Abstract  

data Initializer dom exp
   = Iint
   | Ibool
   | Idouble
   | Idomain dom
   | Ituple [Initializer dom exp]
   | Iarray dom (Initializer dom exp)
   | Ilist Int (Initializer dom exp)
   | Iset dom exp exp 
   
data Technique = SAT | SMT | Narrowing | IP  deriving Show

data Constraint
   = None Formula
   | One Formula
   | Some Formula
   | Full Formula
   | Subset Formula Formula
   | FunDep Formula [String] [String]
   | RightArrow Formula Formula
   | Fact Formula
   | BoolC String [Constraint]
   | All [String] Formula Constraint
   
data Prog = Prog [Decl]

--------------------------------------

valueP (ELit pos l) = True
valueP (EVar n) = True
valueP (EAbs x) = True
valueP (ETuple kind es) = all valueP es
valueP e = False

--------------------------------------
-- names

name (Nm (s,p)) = s
rename f (Nm(x,p)) = Nm(f x,p)
toName s = Nm(s,noPos)

samePos (Nm(_,pos)) s = Nm(s,pos)

instance Loc Name where
  loc (Nm(s,p)) = p
  
instance Eq Name where
  (Nm(s,_)) == (Nm(t,_)) = s==t

instance Show Name where
  show (Nm(x,p)) = x

-------------------------------------------
--------------------------------------
-- free variables

-- Free variables in Atom and formula arise because of
-- escaped things  e.g.  f(x,$(e))  or $(e)(x,y,z)

freeAtom (Prim nm pat) ans = insert (name nm) free
  where (free,bound) = patBinds pat (ans,[])
freeAtom (Escape pos e pat) ans = freeExp e bound
  where (free,bound) = patBinds pat (ans,[])

freeForm (Atomic a) ans = freeAtom a ans
freeForm (Conj x y) ans = freeForm x (freeForm y ans)
freeForm (Disj x y) ans = freeForm x (freeForm y ans)
freeForm (Negation y) ans = (freeForm y ans)
freeForm (Embed ps e) ans = freeForm e ans
freeForm (Equality p1 n p2) ans = insert (name n) free2
   where (free1,bound1) = patBinds p1 (ans,[])
         (free2,bound2) = patBinds p2 (free1,bound1)

-- freeDec d ans ---> (appear free,binds)

freeDec (Def _ p e) ans = (freeExp e ans,binds)
  where (ans2,binds) = patBinds p (ans,[])  -- it should be that ans2==ans
freeDec (Rule2 p nm rhs) ans = (error ("Not yet freeDec") ans,[nm])
freeDec (DataDec p nm cs) ans = (error ("Not yet freeDec") ans,nm:(map fst cs))
freeDec (FunDec p nm rhs) ans = (error ("Not yet freeDec") ans,[nm])
freeDec (Domain p base nm elems) ans = (freeExp elems ans,[nm])
freeDec (Find p pairs wherecl strat tech) ans = (ans4,bound)
  where g (p,e) (free,bound) = (freeInit e free2,bound2)
           where (free2,bound2) = patBinds p (free,bound) 
        (ans2,bound) = foldr g (ans,[]) pairs
        ans3 = (freeExp wherecl ans2) \\ bound
        ans4 = freeStrat strat ans3
        
freeInit:: Initializer Expr Expr -> [String] -> [String]
freeInit (Idomain e) ans = freeExp e ans
freeInit (Iarray e i) ans = freeExp e (freeInit i ans)
freeInit (Ilist n i) ans = freeInit i ans
freeInit (Ituple xs) ans = foldr freeInit ans xs
freeInit (Iset i x y) ans = 
    freeExp i (unionL ans (freeExp x (freeExp y []) \\ ["none","full"]))
freeInit x ans = ans

freeStrat:: Strategy -> [String] -> [String]
freeStrat (Many e) ans = freeExp e ans
freeStrat (Max e) ans = freeExp e ans
freeStrat (Min e) ans = freeExp e ans
freeStrat x ans = ans

decNames d = snd(freeDec d [])

freeExp (ELit _ _) ans = ans
freeExp (EVar n) ans = insert (name n) ans
freeExp (EApp f xs) ans = foldr freeExp (freeExp f ans) xs
freeExp (EAbs ps) ans = foldr g ans ps
  where g (p,e) ans = freeExp e ans2 \\ bound
          where (ans2,bound) = patBinds p (ans,[])          
freeExp (ELet d e) ans = (freeExp e ans2) \\ bound where (ans2,bound) = freeDec d ans
freeExp (ETuple kind xs) ans = foldr freeExp ans xs
freeExp (EForm (Left form)) ans = ans
freeExp (EForm (Right constr)) ans = ans
freeExp (ESet xs e) ans = freeExp xs (freeExp e ans)
freeExp (EComp c) ans = freeComp c ans

patBinds:: Pat -> ([String],[String]) -> ([String],[String])
patBinds (PVar n) (free,bound) = (free,insert (name n) bound)
patBinds (PLit pos _) ans = ans
patBinds (PTuple tk ps) ans = foldr patBinds ans ps
patBinds (PCon nm ps) ans = foldr patBinds ans ps
patBinds (PEsc e) (free,bound) = (freeExp e free,bound)
   -- PEsc should only appear in patterns inside formula
patBinds (PWild pos) ans = ans

freeComp (Range x y) ans = freeExp x (freeExp y ans)
freeComp (Comp e ts) ans = (freeExp e ans2) \\ bound
  where (ans2,bound) = foldr freeCompTerm (ans,[]) ts

freeCompTerm (Pred b) (ans,bound) = (freeExp b ans \\ bound, bound)
freeCompTerm (Generator p e) (free,bound) = 
         (freeExp e free \\ bound,morebound)
  where (morefree,morebound) = patBinds p (free,bound)
  
---------------------------------------------------------------
-- How to deal with N-tuples

patTuple :: [Pat] -> Pat             -- Form a Pat like (x,y:ys)
patTuple [] = PLit prelude LUnit     -- (x,y,z,w) --> (x,(y,(z,w)))
patTuple [p] = p
patTuple ps = PTuple TUPLE ps

patterns (PTuple TUPLE ps) = ps
patterns p = [p]

expTuple :: [Expr] -> Expr           -- Form an Expression with is a tuple like (3,4-9)
expTuple [] = ELit prelude LUnit
expTuple [p] = p
expTuple es = ETuple TUPLE es

prelude = newPos "Preude" 0 0
-- Making Patterns and Expressions

truePat  = PCon (Nm("True",prelude)) []
falsePat = PCon (Nm("False",prelude)) []
patNil = PCon (Nm("[]",prelude)) []
pConsUp pnil [] = pnil
pConsUp pnil (p:ps) = PCon (Nm(":",prelude)) [p,pConsUp pnil ps]

listP ps = pConsUp patNil ps
consPat p ps =  PCon (Nm(":",prelude)) [p,ps]


unitExp = ELit noPos LUnit
consExp x y = EApp (EVar (Nm(":",prelude))) [x,y]
nilExp = (EVar (Nm("[]",prelude)))
listExp = foldr consExp nilExp
trueExp = EVar (Nm("True",prelude))
falseExp = EVar (Nm("False",prelude))

caseExp x ms = EApp (EAbs ms) [x]
ifExp x y z = caseExp x [(truePat,y),(falsePat,z)]

applyE :: [Expr] -> Expr
applyE [t] = t
applyE [x,y] = EApp x [y]
applyE (f:xs) = EApp f xs

toBinary f [x] = (EApp f [x])
toBinary f (x:xs) = toBinary (EApp f [x]) xs

fromBinary (EApp f [x]) = (g,ys++[x]) where (g,ys) = fromBinary f
fromBinary f = (f,[])

binop nm e1 e2 = EApp (EVar nm) [e1,e2]

isChar (ELit _ (LChar _)) = True
isChar _ = False
charOf (ELit _ (LChar c)) = c

isList (EApp (EVar (Nm(":",_))) [x,y])
   = do { ys <- isList y; return (x:ys)}
isList (EVar (Nm("[]",_))) = Just []
isList _ = Nothing


--------------------------- Computing Shapes -------------------------
-- When evaluating Formula, we match patterns against Literals
-- And we must also keep track of the shape of the tuples in the sets.
-- Every element of the tuple is associated with a variable and 
-- a dimension. This is the role of Shape. Every operator computes
-- a result Shape from the Shape of its inputs. This section 
-- describes these Shape computations

type Shape = [(String,Dimension)] 
type Args = [(Pat,Dimension)]
type Projection = [Int]  -- A projection picks a subset from a Shape or an Arg

type Sub1 = [(String,Literal)]  -- maps Variables to Literals
type Info b = (Shape,[([Literal],b)]) -- the ([Literal],b) is one element of s Set
type SubAns b =  (Shape,[(Sub1,b)])

-- Creating Sub1 substitutions by matching
matchLit:: Sub1 -> Pat -> Literal -> Maybe Sub1
matchLit s (PVar nm) v = Just((name nm,v):s)
matchLit s (PWild pos) v = Just s
matchLit s (PLit pos c) v = if v==c then Just s else Nothing
matchLit s (PCon (Nm(":",_)) [x,y]) (LString (z:zs)) = 
  do { s1 <- matchLit s x (LChar z)
     ; matchLit s1 y (LString zs) }
matchLit s (PCon (Nm("[]",_)) []) (LString ("")) = Just s     
matchLit s (PCon (Nm(c1,_)) []) (LCon _ c2) | c1==c2 = Just s
matchLit s (PEsc e) v = error "No PEsc in matchLit"
matchLit s p v = Nothing

-- Where (at what index) does 'nm' appear in the list?
-- And, what dimension does it have?

location n nm [] = Nothing
location n nm ((name,d):more) = 
  if name==nm then Just(name,d,n) else location (n+1) nm more

locate nm sh1 = 
  case location 0 nm sh1 of 
    Nothing -> error ("Name not found in projection: "++nm)
    Just t -> t

-- Projection Shapes
-- f(x,y,3,x,z) ---> ((x,y,z),[0,1,4]) 
-- Both literals and duplicates are removed by the projection.

projShape:: Args -> (Shape,Projection)
projShape xs = help 0 xs where
   help n [] = ([],[])
   help n ((PVar nm,d):ps) = ((name nm,d):qs,n:ns) where (qs,ns) = help (n+1) ps
   help n ((p,d):ps) = help (n+1) ps

-- Conjunction Shapes
-- f(x,y),g(y,z,x) ----> (z,y,x)  
-- Any consistent order is OK, we choose this order because it is convenient

conjShape :: Shape -> Shape -> Shape
conjShape ans [] = ans
conjShape ans ((p,d):ps) = 
  case lookup p ans of
    Nothing -> conjShape ((p,d):ans) ps
    Just _ -> conjShape ans ps

-- Disjunction Shapes
-- f(x,y,z) ;  g(w,y,x)  ----> ([x,y],[0,1],[2,1]) 
-- project [0,1] [x,y,z] = [x,y]    
-- project [2,1] [w,y,x] = [x,y]

disjShape :: Int -> Shape -> Shape -> (Shape,Projection,Projection)
disjShape n [] shape = ([],[],[])
disjShape n ((nm,d):more) shape =
  case location 0 nm shape of
    Nothing -> disjShape (n+1) more shape
    Just (pat,dim,m) -> ((pat,dim):sh2,n:lproj,m:rproj) 
       where (sh2,lproj,rproj) = disjShape (n+1) more shape


-- f(x,y) <- g(a,x,y,b)  ---->  ([x,y],[1,2])
       
lhsProjShape:: Shape -> Shape -> (Shape, Projection)
lhsProjShape sh1 pairs = (sh2,ps)
  where triples = map (\ (nm,d) -> locate nm sh1) pairs
        sh2 = map (\ (p,d,n) -> (p,d)) triples
        ps = map (\ (p,d,n) -> n) triples


lhsProjShape2:: Shape -> [String] -> (Shape, Projection)
lhsProjShape2 sh1 pairs = (sh2,ps)
  where triples = map (\ (nm) -> locate nm sh1) pairs
        sh2 = map (\ (p,d,n) -> (p,d)) triples
        ps = map (\ (p,d,n) -> n) triples        


-- used when evaluating a conjunctive formula        
joinShape:: SourcePos -> Shape -> Shape -> (Int,Shape,Projection,Shape,Projection)
joinShape pos sh1 sh2 = (length xs,f1 xs sh1,f2 xs sh1,f3 xs sh2,f4 xs sh2)
  where xs = [ (nm1,d1,i,j) 
             | (i,(nm1,d1)) <- zip [0..] sh1
             , (j,(nm2,d2)) <- zip [0..] sh2
             , ok (nm1,d1) (nm2,d2)
             ]
        ok (nm1,d1) (nm2,d2) = 
            case (nm1==nm2,d1==d2) of
             (True,True) -> True
             (False,_) -> False
             (True,False) -> error (near pos++"join column "++nm1++" has different domains\n   "++show d1++"\n   "++show d2)
        f1 xs sh1 = fill (map (\ (nm,d,i,j) -> (nm,d)) xs) sh1
        f3 xs sh2 = fill (map (\ (nm,d,i,j) -> (nm,d)) xs) sh2
        f2 xs sh1 = fill (map (\ (nm,d,i,j) -> i) xs) [0.. length sh1 - 1]
        f4 xs sh2 = fill (map (\ (nm,d,i,j) -> j) xs) [0.. length sh2 - 1]
        
fill xs [] = xs
fill xs (y:ys) = if elem y xs then fill xs ys else fill (xs++[y]) ys
        
------------- Instantiating things with a substitution  ---------------
-- This is the mechanism for evaluating escapes ($e) in formulas.

{-
-- Instantiate a Formula with a Sub1 substitution
subForm:: Sub1 -> Formula -> Formula
subForm s (Atomic (Prim name pat)) = 
   Atomic(Prim name (substPat1 s pat)) 
subForm s (Atomic (Escape pos exp pat)) = 
   Atomic(Escape pos exp (substPat1 s pat))
subForm s (Conj x y) = Conj (subForm s x) (subForm s y)
subForm s (Disj x y) = Disj (subForm s x) (subForm s y)
subForm s (Negation x) = Negation (subForm s x) 
subForm s (Embed vs f) 
  | any (\ (u,lit) -> elem u vs) s 
  = error ("Bad Embed in subForm, projection variable is in substitution.")
subForm s (Embed vs f) = Embed vs (subForm s f)
subForm s (Equality p1 nm p2) = Equality (substPat1 s p1) nm (substPat1 s p2)

-- Instantiate a constraint with a Sub1 substitution
subCon s (None x) = None(subForm s x)
subCon s (One x) = One(subForm s x)
subCon s (Some x) = Some(subForm s x)
subCon s (Full x) = Full (subForm s x)
subCon s (Subset x y) = Subset(subForm s x)(subForm s y)
subCon s (RightArrow x y) = RightArrow(subForm s x)(subForm s y)
subCon s (FunDep f xs ys)
  | (any (\ (u,lit) -> elem u xs) s) || (any (\ (u,lit) -> elem u ys) s)
  = error ("Bad FunDep in subCon, dependency variable is in substitution.")
subCon s (FunDep f xs ys) = FunDep (subForm s f) xs ys
subCon s (Fact x) = Fact(subForm s x)
subCon s (BoolC name cs) = BoolC name (map (subCon s) cs)
subCon s (All vs form con) = All vs (subForm s form) (subCon (strip vs s) con)
  where strip vs s = filter (\ (u,lit) -> not(elem u vs)) s
-}

-- Instantiate a Pat with a Sub1 substitution
substPat1:: Sub1 -> Pat -> Pat
substPat1 [] (PVar nm) = PVar nm
substPat1 ((x,v):s)      (PVar nm) | (x == name nm) = PLit (loc nm) v
substPat1 ((x,LCon tn c):s) (PVar nm) | (x == name nm) = PCon (Nm(c,loc nm)) []
substPat1 ((x,v):s)      (PVar nm) | (x == name nm) = error ("Non literal in substPat: "++show v)
substPat1 (_:s) (v@(PVar nm)) = substPat1 s v
substPat1 s (PWild pos) = (PWild pos)
substPat1 s (PLit pos c) = PLit pos c
substPat1 s (PCon c []) = PCon c []
substPat1 s (PTuple k ps) = PTuple k (map (substPat1 s) ps)
substPat1 s p = error ("Non-literal pattern in substPat1: "++show p)

-- f(x,y,3,x,z) ---> (\ [x0,x1,x2,x3,x4] -> x0==x3 && x2==3)
-- The Args determines a Boolean function applied to the Literals 
-- that define a key to a FiniteSet (mapping keys to propositions)

shapeToPred :: Args -> Key -> Bool
shapeToPred args key = help args (tuple key) where
   help [] [] = True
   help ((PVar s,d):xs) (k: ks) =    help (map sub xs) ks
      where sub (p,d2) = (substPat1 [(name s,k)] p,d2)
   help ((PWild pos,d):xs) (k: ks) = help xs ks
   help ((PLit pos lit,d):xs) (k: ks) = (lit==k) && (help xs ks)
   help ((PCon (Nm(c,pos)) [],d):xs) (k: ks) = help ((PLit pos (LCon (dimTypeName d) c),d):xs) (k:ks)
   help ((PEsc e,d):xs) (k: ks) = error "No PEsc in shape to Pred"
   help _ _ = False

dimTypeName (Dim n base xs) = show base


--------------- Computing a cross product ----------------------

-- For every possible pair of tuples (1 from each set) that match
-- create a substitution that binds the variables in the shape

crossProd:: (BooleanM m b) =>
     (Shape, [(Key, b)]) -> (Shape, [(Key, b)]) -> m (Shape, [(Sub1, b)])
crossProd (sh1,xs) (sh3,ys) = do { pairs <- foldrM acc [] triples; return (sh4,pairs)}
  where acc (s2,b1,b2) ans = do { b3 <- conjM b1 b2; return((s2,b3):ans)}
        sh2 = conjShape [] sh1
        sh4 = conjShape sh2 sh3
        triples = [ (s2,b1,b2) 
                  | (key1,b1) <- xs
                  , Just s1 <- [thread1 [] sh1 (tuple key1)]
                  , (key2,b2) <- ys
                  , Just s2 <- [thread1 s1 sh3 (tuple key2)] 
                  ]

thread1 :: Sub1 -> Shape -> [Literal] -> Maybe Sub1
thread1 s [] [] = Just s
thread1 s ((p,d):more) (v:vs) = 
   case matchLit s (substPat1 s (PVar (Nm(p,noPos)))) v of
     Just s2 -> thread1 s2 more vs
     Nothing -> Nothing

subsToAns:: (Show a, BooleanM m a) => (Shape,[(Sub1,a)]) -> m (Shape, FiniteSet a)
subsToAns (shape,xs) = do { set <- fromListWithM conjM list2; return(shape,FA ds set)}
  where ds = map snd shape  -- the dimensions
        list2 = map f xs
        f (sub,b) | length shape ==length sub = (toKey "subsToAns" ds (zipWith g shape sub),b)
        f (sub,b) = error ("Shape and Sub lengths don't match: "++showshapes shape++" "++show sub++" "++show b)
        g (name,d) (nm,lit) | name==nm = lit
        g (pat,d) (nm,lit) = error ("Inconsistent shape "++show pat++" "++nm)  
        showshapes xs = plistf hh "[" xs "," "]"
        hh (nm,d) = "("++show nm++","++dimName d++")"


------------------------------------------
-- Class instances

instance Show Pat where
  show (PVar nm) = name nm
  show (PLit pos l) = show l
  show (PWild pos) = "_"
  show (PTuple TUPLE []) = "()"
  show (PTuple TUPLE [x]) = show x
  show (PTuple TUPLE xs) = "("++f xs++")"
    where f [x] = show x
          f (x:xs) = show x++","++f xs
  show (PTuple DIM xs) = "#("++f xs++")"
    where f [x] = show x
          f (x:xs) = show x++","++f xs          
  show (PCon c []) = name c
  show (PCon c xs) = "("++name c++" "++f xs++")"
    where f [x] = show x
          f (x:xs) = show x++" "++f xs   
  show (PEsc (EVar x)) = "$"++name x
  show (PEsc x) = "$("++show x++")"
 
instance Eq Pat where
  (PVar x) == (PVar y) = name x == name y
  (PLit _ x) == (PLit _ y) = x==y
  -- (PSum i x)== (PSum j y) = i==j && x==y
  (PTuple i xs) == (PTuple j ys) = i==j && xs == ys
  (PWild _) == (PWild _) = True
  (PCon c xs) == (PCon d ys) = c==d && xs == ys
  (PEsc x) == (PEsc y) = error "No PEsc yet in equality"
  _ == _ = False
 

------------------------------------------------------------
-- pretty printing

-- For Formulas

ppFormParen :: Formula -> Doc
ppFormParen (x@(Conj _ _)) = PP.parens(ppForm x)
ppFormParen (x@(Disj _ _)) = PP.parens(ppForm x)
ppFormParen (x@(Negation _)) = PP.parens(ppForm x)
ppFormParen x = ppForm x

ppAtom (Prim n args) = text (name n) <> ppArgs args
ppAtom (Escape pos nm args) = ppEsc nm <> ppArgs args

ppEsc (EVar s) = text("$"++ name s)
ppEsc e = text "$" <> ppParExp e

ppArgs (p@(PTuple _ _)) = ppPat p
ppArgs p = PP.parens(ppPat p)

ppForm :: Formula -> Doc
ppForm (Atomic a) = ppAtom a
ppForm (Embed (args) form) = 
  PP.braces (PP.fcat[ ppSep "," (map text args)
                    , text " <- ", ppForm form])
ppForm (Conj x y) = PP.fcat [bracket x, text ", ",bracket y]
  where bracket (x@(Disj _ _)) = PP.parens(ppForm x)
        bracket x = ppForm x
ppForm (Disj x y) = PP.fcat [bracket x, text "; ",bracket y]
  where bracket (x@(Conj _ _)) = PP.parens(ppForm x)
        bracket x = ppForm x
ppForm (Negation x) = text "!" <> ppFormParen x
ppForm (Equality p1 nm p2) = ppArgs p1 <+> text("=#"++name nm) <+> ppArgs p2

ppSep comma xs = PP.parens(PP.hcat(PP.punctuate (text comma) xs))
ppSep2 comma xs = (PP.hcat(PP.punctuate (text comma) xs))

instance Show Formula where
  show x = render(ppForm x)
  
instance Show Atom where
  show x = render(ppAtom x)
 
------------------------------------------------------
-- Pretty printing patterns

ppLit :: Literal -> Doc
ppLit (LInt n) = text(show n)
ppLit (LDouble d) = text (show d)
ppLit (LString s) = text (show s)
ppLit (LChar c) = text(show c)
ppLit (LUnit) = text "()"
ppLit (LCon tn x) = text (show x)

ppPat pat =
  case pat of
    PLit p l -> ppLit l
    PVar v -> text (name v)
    PTuple x ps -> ppTKind x <> ppSep "," (map ppPat ps)
    PWild p -> text "_"
    PCon (Nm(v,pos)) [] -> text v
    PCon (Nm(":",_)) (p1:ps) -> PP.parens $ ppPat p1 <+> text  ":" <+> PP.hsep (map ppPat ps)
    PCon (Nm(v,pos)) ps -> PP.parens $ text v <+> PP.hsep (map ppPat ps)
    PEsc e -> ppEsc e
----------------------------------------------------------------------
-- Pretty printing Expr, and various strategies for parenthesizing

ppParFun (x@(EApp _ _)) = ppExp x
ppParFun x = ppParExp x

ppParExp (x @ (EVar _)) = ppExp x
ppParExp (x @ (ELit _ _)) = ppExp x
ppParExp (x @ (ETuple _ _)) = ppExp x
-- ppParExp (x @ (EChoose Choose _ _)) = ppExp x
ppParExp (x @ (EForm _)) = ppExp x
ppParExp x = case isList x of
             Just z -> ppList z
             Nothing -> PP.parens $ ppExp x    

ppList xs | all isChar xs = PP.doubleQuotes $ text (map charOf xs)
ppList xs = PP.brackets(PP.fsep (sepByPP ppExp "," xs))  

sepByPP f comma [] = []
sepByPP f comma [x] = [f x]
sepByPP f comma (x:xs) = ((f x)<>(text comma)):sepByPP f comma xs

ppTKind TUPLE = PP.empty
ppTKind DIM = text "#"
    
ppExp e =
  case e of
    ELit pos l -> ppLit l
    EVar (Nm(v,pos)) -> text v
    (EApp (EAbs ms) [e]) -> 
       (text "case" <+> ppParExp e <+> text "of") $$
                 (PP.nest 2 (PP.vcat (map ppMatch ms)))
    (term@(EApp (EVar (Nm(f,pos))) [x,y]))
        | Just ys <- isList term -> ppList ys
        | infixp f -> PP.sep [(ppParFun x),text f,ppParFun y]
    EApp f xs -> ppSep2 " " (ppParFun f : (map ppParExp xs))    
    EAbs ms -> (text "\\" $$
                 (PP.nest 2 (PP.vcat (map ppMatch ms))))
    ETuple kind ps -> ppTKind kind <> ppSep "," (map ppExp ps)
    ESet ds expr ->
      PP.sep[text "set",ppExp ds,ppExp expr]
    (e@(ELet _ _)) ->
        PP.vcat [text "let "<> ppDec d
                , PP.nest 4 (PP.vcat(map ppDec ds))
                ,text "in" <+> ppExp body]
      where (d:ds,body) = flatLet e []
    EComp x -> ppComp x 
    EForm (Left form) -> text "$(" <+> ppForm form <+> text ")"
    EForm (Right c) -> text "$(" <+> ppCon c <+> text ")"
      

ppComp:: Comprehension -> Doc
ppComp (Range e1 e2) = 
  PP.sep[text "[" <> ppExp e1, text "..", ppExp e2 <> text "]"]
ppComp (Comp x ms) =
  PP.brackets(PP.sep[ppExp x, text "|",PP.sep(PP.punctuate (text ",") (map ppCompTerm ms))])

ppCompTerm (Pred e) = ppExp e
ppCompTerm (Generator p e) = 
  PP.sep [ ppPat p, text "<-", ppExp e]
    
flatLet (ELet d e) ds = flatLet e (d:ds)
flatLet e ds = (ds,e)
    
ppMatch (p,body) = PP.sep [ppPat p <+> text "->",PP.nest 2 (ppExp body)]

instance Show Expr where
  show d = render(ppExp d)

-----------------------------------------------------------
-- Pretty printing Decl and Program

ppDec :: Decl -> Doc
ppDec (Def _ pat exp) = PP.sep [ppPat pat
                               ,PP.nest 3 (text "=")
                               ,PP.nest 3 (ppExp exp)]      
ppDec (Domain _ base nm exp) = 
  PP.sep[text "dim",text (show base), text nm,text "=",ppExp exp]
ppDec (DataDec _ nm cs) = (text "data" <+> text nm <+> text "=") <+> ppSep2 " | " (map f cs)
  where f (c,xs) = text c <+> PP.sep (map g xs)
        g Nothing = text "_"
        g (Just c) = text c
ppDec (FunDec _ nm cls) = PP.vcat(map f cls)
  where f (ps,e) = PP.hsep [text nm <+> PP.hsep(map ppPat ps), text "=", ppExp e]
ppDec (Rule2 _ nm rhs) = ppRuleRhs (text nm) rhs
ppDec (Find _ ds constraint strat tech) =
   PP.vcat[text "exists" $$ PP.nest 7 (PP.vcat(map f ds))
          ,text " where" <+> ppExp constraint
          ,text "  find" <+> ppStrat strat
          ,text "    by" <+> text(show tech)]
 where f (nm,x) = text (show nm) <> text ":" <+> ppInit ppExp ppExp x

ppInit:: (a -> Doc) -> (b -> Doc) -> Initializer a b -> Doc
ppInit f g Iint = text "Int"
ppInit f g Ibool = text "Bool"
ppInit f g Idouble = text "Double"
ppInit f g (Idomain e) = f e
ppInit f g (Iarray e i) = text "Vector" <+> f e <+> ppInit f g i
-- ppInit f g (Imatrix e i) = text "Matrix" <+> f e <+> ppInit f g i 
ppInit f g (Ilist n i) = text "List" <+> PP.int n <+> ppInit f g i 
ppInit f g (Ituple xs) = ppSep "," (map (ppInit f g) xs)
ppInit f g (Iset i x y) = PP.sep[text "set",f i, g x, text "..", g y]
 

ppStrat First = text "First"
ppStrat (Many e) = text "Many" <+> ppExp e
ppStrat (Max e) = text "Max" <+> ppExp e
ppStrat (Min e) = text "Min" <+> ppExp e
ppStrat Abstract = text "Abstract"

ppRuleRhs lhs (DeclRhs xs) = ppdecl lhs xs
ppRuleRhs lhs (DefRhs args f) = 
   PP.sep[lhs<>PP.parens(PP.cat (PP.punctuate (text ",") (map text args))),text "<-",ppForm f <> text "."]
ppRuleRhs lhs (Both xs f) =
  PP.vcat[ppdecl lhs xs,PP.sep[lhs<>pparg xs,text "<-",ppForm f <> text"."]]

ppdecl lhs xs = PP.sep([lhs<>(pparg xs),text "->"]++(PP.punctuate (text ",") (map f xs)))<>text "."
  where f (arg,pred) = ppExp pred<>PP.parens(text arg)
pparg xs = PP.parens(PP.cat (PP.punctuate (text ",") (map f xs)))
  where f (arg,pred) = text arg

instance Show (Initializer Expr Expr) where
  show x = render(ppInit ppExp ppExp x)
  

instance Show Decl where
  show d = render(ppDec d)

instance Show Prog where
  show (Prog xs) = render(PP.vcat(map h xs))
    where h x = PP.vcat[ppDec x,text ""]
    
-----------------------------------------------------------
ppCon:: Constraint -> Doc
ppCon (None f) = text "none" <+> ppForm f
ppCon (One f) = text "one" <+> ppForm f
ppCon (Some f) = text "some" <+> ppForm f
ppCon (Full f) = text "full" <+> ppForm f
ppCon (Subset x y) = PP.sep[ppForm x,text "<=", ppForm y] 
ppCon (RightArrow x y) = PP.sep[ppForm x,text "->", ppForm y] 
ppCon (FunDep f dom rng) =
     PP.sep[ppForm f, text "|",ppNames dom,text "->",ppNames rng]
  where ppNames [x] = text x
        ppNames xs = ppSep "," (map text xs)
ppCon (Fact f) = ppForm f   
ppCon (BoolC str xs) = 
     PP.parens(PP.sep(PP.punctuate (text (" "++str)) (map ppCon xs)))
ppCon (All args f c) = 
    PP.parens(PP.sep[text "forall",PP.sep[ parensN(map text args),text "<-",ppForm f],text ".",ppCon c])

parensN [x] = x
parensN xs = ppSep "," xs
      
instance Show Constraint where
  show d = render(ppCon d)

------------------------------------------------------
-- Defining Infix and Prefix operators
-- Create a datastructure parameterized by two functions
-- Use (1) obtain a list of all operators
--     (2) Create a structure for input to buildExpressionParser

opList prefix infiX =
    [ [ prefix "~" ]
    , [ infiX "!!" AssocLeft, infiX "." AssocLeft]
    , [ infiX "^"  AssocRight]
    , [ infiX "*"  AssocLeft, infiX "/"  AssocLeft]
    , [ infiX "+"  AssocLeft, infiX "-"  AssocLeft]
    , [ infiX ":" AssocRight]
    , [ infiX "++" AssocRight]
    , [ infiX "==" AssocNone, infiX "/=" AssocNone, infiX "<"  AssocNone
      , infiX "<=" AssocNone, infiX ">"  AssocNone, infiX ">=" AssocNone ]
    , [ infiX "<=>" AssocNone ]
    , [ infiX "&&" AssocRight ]
    , [ infiX "||" AssocRight, infiX "=>" AssocRight ]
    , [ infiX "$" AssocRight ]
    , [ infiX "?" AssocRight]
   ]

-- Use (1)

metaDatalogOps = filter (/="") (concat (opList prefix op))
  where prefix x = ""
        op x y = x  
        
infixp s = elem s metaDatalogOps      
  

getname (nm,uniq,v) = nm
getpair (nm,uniq,v) = (nm,v)
getuniq (nm,uniq,v) = uniq
 

decName (Def pos pat exp) = show pat
decName (Rule2 pos nm rhs) = nm
decName (Domain pos typ nm exp) = nm
decName (DataDec pos t cs) = show t
decName (FunDec pos nm cls) = nm
decName (Find pos zs e strat tech) = plistf (show . fst) "" zs "," ""

---------------------------------------------------

class Loc t where
  loc:: t -> SourcePos

instance Loc Expr where loc = expLoc
instance Loc SourcePos where loc = id
instance Loc Decl where loc = decLoc
instance Loc Pat where loc = patLoc
instance Loc Atom where loc = atomLoc
instance Loc Formula where loc = formLoc

near :: (Loc t) => t -> String
near exp = "near "++show(loc exp)++"\n"

expLoc:: Expr -> SourcePos
expLoc (EVar (Nm(nm,pos))) = pos
expLoc (ELit p l) = p
expLoc (ETuple kind xs) = expLoc (head xs)
expLoc (EApp x y) = expLoc x
expLoc (EAbs zs) = expLoc (snd(head zs))
expLoc (ESet x y) = expLoc x
-- expLoc (EChoose _ x y) = expLoc x
expLoc (EForm (Left x)) = formLoc x
expLoc (EForm (Right x)) = conLoc x
expLoc x = noPos

conLoc (None f) = formLoc f
conLoc (One f) = formLoc f
conLoc (Some f) = formLoc f
conLoc (Full f) = formLoc f
conLoc (Subset f g) = formLoc f
conLoc (FunDep f lhs rhs) = formLoc f
conLoc (RightArrow f g) = formLoc f
conLoc (Fact f) = formLoc f
conLoc (BoolC s (x:xs)) = conLoc x
conLoc (BoolC s []) = noPos
conLoc (All names f c) = formLoc f


decLoc (d@(Def pos pat exp)) = pos
decLoc (Rule2 pos nm (DeclRhs xs)) = pos
decLoc (d@(Domain pos typ nm exp)) = pos
decLoc (DataDec pos t cs) = pos
decLoc (FunDec pos nm cls) = pos
decLoc _ = noPos         

patLoc (PVar nm) = loc nm
patLoc (PWild pos) = pos
patLoc (PLit pos c) = pos
patLoc (PCon (Nm(c1,p)) []) = p
patLoc (PEsc e) = expLoc e
patLoc (PTuple _ []) = noPos
patLoc (PTuple _ ps) = patLoc (head ps)

atomLoc :: Atom -> SourcePos
atomLoc (Prim n p) = loc n
atomLoc (Escape p _ _) = p

formLoc (Atomic x) = atomLoc x
formLoc (Conj x y) = formLoc x
formLoc (Disj x y) = formLoc x
formLoc (Negation x) = formLoc x
formLoc (Embed x f) = formLoc f
formLoc (Equality p1 n p2) = patLoc p1


 
--------------------------------------------------------------------------

form1 = (Embed ([("x"),("z")]) -- PTuple TUPLE[PVar "x",PVar "z"])
        (Conj (Atomic (Prim (Nm("parent",noPos)) (PTuple TUPLE [PVar (Nm("x",noPos)),PVar (Nm("y",noPos))])))
              (Atomic (Prim (Nm("parent",noPos)) (PTuple TUPLE [PVar (Nm("y",noPos)),PVar (Nm("z",noPos))])))))

proj23 (a,b,c) = (a,c)

-- tt0 = evalForm env1 form1
tt3 = disjShape 0 [("x",pd),("y",pd),("z",pd)] [("w",pd),("y",pd),("x",pd)]
   
-- ========================================================================

  