module Types where

-- Get the FIO monad
import Monads
import Value (Base(..))
import Simple (plistf)

-- Useful functions
import Data.List(union,unionBy,any,(\\),partition)
import Data.IORef(newIORef,readIORef,writeIORef,IORef)

-- These are for pretty printing
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($$),($+$),render)

------------------------------------------------

type Pointer t = IORef (Maybe t)
type Uniq = Integer

data Typ 
   = TyVar String Kind
   | TyApp Typ Typ
   | TyTuple Kind [Typ]   --  Kind is either Star or DomKind
   | TyCon String Kind
   | TcTv (Uniq,Pointer Typ,Kind)

data Kind   
   = Star 
   | DomKind Base
   | Karr Kind Kind
   | Kvar (Uniq,Pointer Kind)
   
data Scheme = Sch [(String,Kind)] Rho

data Rho 
   = Tau Typ
   | Rarr Scheme Rho


---------------------------------------------  
instance Eq Kind where
  Star == Star = True
  DomKind s == DomKind t = s==t
  (Karr x y) == (Karr a b) = x==a && y==b
  (Kvar (i,_)) == (Kvar (j,_)) = i==j
  _ ==_ = False

-----------------------------------------------
-- pretty printing

-- tupleTypes (TyApp (TyApp (TyCon "(,)" _) x) y) = x : tupleTypes y
tupleTypes (TyTuple k xs) = xs
tupleTypes x = [x]

ppTyp :: Typ -> Doc
ppTyp (TyVar s k) = text s
ppTyp (TyCon c k) = text c
ppTyp (TyTuple k ts) = bracket(PP.cat (PP.punctuate PP.comma (map ppTyp ts)))
  where bracket = case k of { Star -> PP.parens; DomKind s -> PP.braces}
{-
ppTyp (t@(TyApp (TyApp (TyCon "(,)" _) x) y)) =
        PP.parens(PP.cat (PP.punctuate PP.comma (map ppTyp (tupleTypes t))))
       --  PP.cat[PP.lparen <> ppTyp x,PP.comma<>ppTyp y,PP.rparen]
-}
ppTyp (TyApp (TyApp (TyCon "(->)" _) x) y) = 
        PP.sep[ppParenTyp x <+> text "->",PP.nest 1 (ppTyp y)]
ppTyp (TyApp (TyCon "[]" _) x) = PP.brackets (ppTyp x)
ppTyp (TyApp f x) | needsParens x = (ppTyp f) <+> (PP.parens (ppTyp x))
ppTyp (TyApp f x) = (ppTyp f) <+> (ppTyp x)
ppTyp (TcTv (uniq,ptr,k)) = text("t"++show uniq)

needsParens (TyTuple k _) = False 
needsParens (TyApp (TyCon "[]" _) x) = False
-- needsParens (TyApp (TyApp (TyCon "(,)" _) _) _) = False
needsParens (TyApp (TyApp (TyCon "(->)" _) _) _) = True
needsParens (TyApp _ _) = True
needsParens _ = False

ppParenTyp (w@(TyApp (TyApp (TyCon "(->)" _) x) y)) = PP.parens (ppTyp w)
ppParenTyp w = ppTyp w

ppKind :: Kind -> Doc
ppKind Star = text "*"
ppKind (DomKind s) = text ("#"++show s)
ppKind (Karr x y) = PP.parens $ PP.hsep [ ppKind x, text "->", ppKind y]
ppKind (Kvar (uniq,ref)) = text ("k"++show uniq)

ppScheme :: Scheme -> Doc
ppScheme (Sch [] t) = ppRho t
ppScheme (Sch vs t) = PP.sep [text "forall", PP.sep (ppAllArgs vs) <+> text ".", ppRho t]

ppAllArgs [(s,Star)] = [text s]  
ppAllArgs [(s,k)] = [PP.parens $ text s <+> text "::" <+> ppKind k]  
ppAllArgs ((s,Star):xs) = text s :(ppAllArgs xs)
ppAllArgs ((s,k):xs) = (PP.parens $ text s <+> text "::" <+> ppKind k):(ppAllArgs xs)
ppAllArgs [] = [PP.empty]

ppRho (Tau x) = ppTyp x
ppRho (Rarr x y) = PP.sep[ppParenSch x <+> text "->",PP.nest 1 (ppRho y)]

ppParenSch (Sch [] (Tau t)) = ppParenTyp t
ppParenSch x = PP.parens (ppScheme x)

instance Show Typ where
  show t = render(ppTyp t)
instance Show Kind where
  show t = render(ppKind t)
instance Show Scheme where
  show t = render(ppScheme t)
instance Show Rho where
  show r = render(ppRho r)
   

----------------------------------------------------------------------
-- for debugging when you need to see all the structure of a type

showT (TyVar s k) = "(Var "++show s++" "++show k++")"
showT (TyCon s k) = "(Con "++show s++" "++show k++")"
showT (TyTuple Star xs) = plistf showT "(Tuple* " xs "," ")"
showT (TyTuple (DomKind s) xs) = plistf showT "(Tuple# " xs "," ")"
showT (TcTv (uniq,ptr,k)) = "(Tv "++show uniq++" "++show k++")"
showT (TyApp f x) = "("++showT f++ " "++showT x++")"

showR (Tau t) = "(Tau "++showT t++")"
showR (Rarr s x) = "(Rarr "++showS s++ " "++showR x++")"

showS (Sch vs r) = "(Sch ... "++showR r++")"
  
-----------------------------------------------
-- operations on Typ

freshType k = 
  do { n <- fio(nextInteger)
     ; p <- fio(newIORef Nothing)
     ; return(TcTv (n,p,k)) }

freshRho k = do { t <- freshType k; return(Tau t)}
freshScheme k = do { r <- freshRho k; return(Sch [] r)}
     
     
prune :: Typ -> FIO Typ
prune (typ @ (TcTv (uniq,ref,k))) =
  do { maybet <- fio(readIORef ref)
     ; case maybet of
         Nothing -> return typ
         Just t -> do{t2 <- prune t; fio(writeIORef ref (Just t2)); return t2}}
prune t = return t

getTyVars:: Typ -> [(String,Kind)] 
getTyVars (TyVar n k) = [(n,k)]
getTyVars (TyCon s k) = []
getTyVars (TyTuple k xs) = foldr acc [] xs
  where acc t ans = unionBy eq (getTyVars t) ans where eq (p1,k1) (p2,k2) = p1==p2
getTyVars (TyApp x y) = unionBy eq (getTyVars x) (getTyVars y) where eq (p1,k1) (p2,k2) = p1==p2
getTyVars (TcTv (uniq,ptr,k)) = []          

get_tvs:: Typ -> FIO ([(Pointer Typ,Kind)],[String])
get_tvs t = do { x <- prune t; f x }
  where f (TyVar n k) = return ([],[n])
        f (TyCon s k) = return ([],[])
        f (TyTuple k xs) = 
          do { pairs <- mapM get_tvs xs
             ; let (vs,us) = unzip pairs
                   eq (p1,k1) (p2,k2) = p1==p2
                   acc x ans = unionBy eq x ans
             ; return(foldr acc [] vs,foldr union [] us)}
        f (TyApp x y) =
          do { (ts,vs) <- get_tvs x 
             ; (ss,us) <- get_tvs y
             ; let eq (p1,k1) (p2,k2) = p1==p2
             ; return(unionBy eq ts ss,union vs us)}
        f (TcTv (uniq,ptr,k)) = return([(ptr,k)],[])             

get_tvsRho (Tau t) = get_tvs t
get_tvsRho (Rarr s r) = 
  do { (ts,vs) <- get_tvsScheme s
     ; (ss,us) <- get_tvsRho r
     ; let eq (p1,k1) (p2,k2) = p1==p2
     ; return(unionBy eq ts ss,union vs us)}

get_tvsScheme (Sch bound r) = 
  do { (vs,us) <- get_tvsRho r
     ; return(vs,us \\ map fst bound) }
  
  
unify :: Loc -> [String] -> Typ -> Typ -> FIO ()
unify loc message x y = do { x1 <- prune x; y1 <- prune y; f x1 y1 }
  where f (t1@(TyVar n k)) (t2@(TyVar n1 k1)) | n==n1 = return ()
        f (TyApp x y) (TyApp a b) = do { unify loc message x a; unify loc message y b }
        f (x@(TyCon s _)) (y@(TyCon t _)) =
           if s==t then return () else matchErr loc ((s++ " =/= " ++t++" (Different type constuctors)") : message) x y 
        f (TyTuple k2 xs) (TyTuple k1 ys) | k1==k2  = unifyL k1 loc message xs ys
        f (TcTv (u1,r1,k1)) (TcTv (u2,r2,k2)) | r1==r2 = return ()
        f (TcTv x) t = unifyVar loc message x t
        f t (TcTv x) = unifyVar loc message x t 
        f s t = matchErr loc ((show s++ " =/= " ++show t++" (Different types)") : message)  s t

unifyL k loc m [] [] = return()
unifyL k loc m (x:xs) (y:ys) = unify loc m x y  >> unifyL k loc m xs ys
unifyL k loc m xs ys = matchErr loc ("different tuple lengths": m) (TyTuple k xs) (TyTuple k ys)

unifyVar loc message (x@(u1,r1,k)) t =
  do { (vs,_) <- get_tvs t
     ; if (any (\(r0,k0) -> r0==r1) vs) 
          then (matchErr loc ("\nOccurs check" : message)  (TcTv x) t)
          else return ()
     ; check loc message t k 
     ; fio(writeIORef r1 (Just t))
     ; return ()
     }

------------
-- Checking that a type has a certain kind

check :: Loc ->  [String] -> Typ -> Kind -> FIO ()
check loc message typ kind = 
  case typ of
    TyVar s k -> unifyK loc (("\nChecking "++s++"::"++show kind++". It has kind "++show k++" instead.") : message) k kind
    TyTuple knd xs -> 
      do { let f n Star = [Star | i <- [1..n]]
               f n (DomKind (Tuple bs)) = (map DomKind bs)
               f 1 k = [k]
               f n k = error ("Tuple kinds don't match in check")
         ; mapM (\ (t,k) -> check loc message t k) (zip xs (f (length xs) knd))
         ; unifyK loc (("Tuple has non-"++show knd++" kind"):message) kind knd
         }    
    TyApp f x -> 
      do { k1 <- freshKind  
         ; check loc message f (Karr k1 kind)
         ; check loc message x k1 }
    TyCon s k -> unifyK loc (("\nChecking type constructor "++s++"::"++show kind) : message) k kind 
    TcTv (uniq,ptr,k) -> unifyK loc (("\nChecking "++show uniq++" has kind "++ show kind) : message) k kind


checkRho:: Loc -> [String] -> Rho -> Kind -> FIO ()
checkRho loc m (Tau t) k = check loc m t k
checkRho loc m (Rarr s r) k = do { checkScheme loc m s Star; checkRho loc m r k}

checkScheme:: Loc -> [String] -> Scheme -> Kind -> FIO ()
checkScheme loc m (Sch vs r) k = checkRho loc m r k

-----------------------------------------------------
-- Monadic substitution, checks that what is subbed matches in kind

tySub :: Loc -> ([(Pointer Typ,Typ)],[(String,Typ)],[(String,Typ)]) -> Typ -> FIO Typ
tySub loc (env@(xs,ys,zs)) x = do { a <- prune x; f a }
  where f (typ@(TyVar s k)) = 
          case lookup s ys of
            Just t -> do { check loc ["\nTySub TyVar: "++s++" check"] t k; return t}
            Nothing -> return typ
        f (TyApp g x) = 
          do { h <- tySub loc env g
             ; y <- tySub loc env x
             ; return(TyApp h y)}
        f (typ@(TyCon c k)) = return(look c zs typ)
        f (TyTuple k xs) = do { ys <- mapM (tySub loc env) xs
                            ; return(TyTuple k ys)}
        f (typ@(TcTv (uniq,ptr,k))) = 
          case lookup ptr xs of
            Just t -> do { check loc ["\nTySub TcTv: "++show uniq ++" check"] t k; return t}
            Nothing -> return typ

rhoSub loc env (Tau t) = do {a <- tySub loc env t; return(Tau a)}
rhoSub loc env (Rarr s r) = do { a <- schemeSub loc env s; b <- rhoSub loc env r; return(Rarr a b)}

schemeSub loc (xs,ys,zs) (Sch vs t) = 
   do { let f (v,k) = do { u <- fresh; return(v,TyVar u k)}
            g (v,TyVar u k) = (u,k)
      ; newys <- mapM f vs
      ; t' <- rhoSub loc (xs,newys++ys,zs) t
      ; return(Sch (map g newys) t')}
    
-----------------------------------------------
-- pure substitution. Does NOT check the kind of things subbed.
-- useful after parsing when parsed things have bad kinds
-- to replace those with well-formed versions

look x xs def =
  case lookup x xs of
    Nothing -> def
    Just t -> t

subTyp :: ([(Pointer Typ,Typ)],[(String,Typ)],[(String,Typ)]) -> Typ -> Typ
subTyp (_,xs,_) (typ@(TyVar s k)) = look s xs typ
subTyp env (TyApp f x) = TyApp (subTyp env f) (subTyp env x)
subTyp env (TyTuple k xs) = TyTuple k (map (subTyp env) xs)
subTyp (_,_,xs) (typ@(TyCon c k)) = look c xs typ
subTyp (xs,_,_) (typ@(TcTv (uniq,ptr,k))) = look ptr xs typ
 
subRho env (Tau x) = Tau (subTyp env x)
subRho env (Rarr s r) = Rarr(subScheme env s) (subRho env r)

subScheme env (Sch vs r) = Sch vs (subRho env r)

-----------------------------------------------------
-- Zonking follows all mutable variable chains and eliminates them

zonk :: Typ -> FIO Typ
zonk t = do { x <- prune t; f x}
  where f (TyVar s k) = do { k1 <- zonkKind k; return(TyVar s k1) }
        f (TyApp f x) = 
          do { g <- zonk f
             ; y <- zonk x
             ; return(TyApp g y)}
        f (TyTuple k xs) = do { ys <- mapM zonk xs;return(TyTuple k ys)}
        f (TyCon c k) = do { k1 <- zonkKind k; return(TyCon c k) }
        f (TcTv (uniq,ptr,k)) =  do { k1 <- zonkKind k; return(TcTv(uniq,ptr,k1)) }

zonkRho (Tau t) = do { a <- zonk t; return(Tau a)}
zonkRho (Rarr x y) = do { a <- zonkScheme x; b <- zonkRho y; return(Rarr a b)}
      
zonkScheme :: Scheme -> FIO Scheme
zonkScheme (Sch vs r) =
  do { let f (v,k) = do { a <- zonkKind k; return(v,a) }
     ; us <- mapM f vs
     ; b <- zonkRho r
     ; return(Sch us b)}

------------------------------------------
-- Turn a scheme into a Rho type, with fresh mutable
-- variables for the universally quantified ones.

instantiate:: Scheme -> FIO Rho
instantiate (Sch xs r) = do { env <- freshen xs; rhoSub noPos ([],env,[]) r }
  where freshen xs = mapM g xs
        g (name,kind) = do { t <- freshType kind; return(name,t) }

rigidize:: Scheme -> FIO([String],Rho)
rigidize (Sch xs r) = do { env <- freshen xs
                         ; ans <- rhoSub noPos ([],env,[]) r
                         ; return(map h env,ans)}
  where freshen xs = mapM g xs
        g (name,kind) = do { n <- fio(nextInteger); return(name,TyVar ("_"++show n) kind) }
        h (name,TyVar nm kind) = nm
        
existInstance:: Scheme -> FIO([String],Rho)
existInstance (Sch xs rho) = 
  do { let range (Tau x) = rng x
           range (Rarr s r) = range r
           rng (TyApp (TyApp (TyCon "(->)" _) x) y) = rng y
           rng x = x
     ; (_,rangevs) <- get_tvs (range rho)
     ; let exists (nm,k) = not(elem nm rangevs)
           (existvs,freshvs) = partition exists xs
           g (name,kind) = do { n <- fio(nextInteger); return(name,TyVar ("_"++show n) kind) }
           h (name,TyVar nm kind) = nm
           f (name,kind) = do { t <- freshType kind; return(name,t) }
     ; env1 <- mapM g existvs
     ; env2 <- mapM f freshvs
     ; ans <- rhoSub noPos ([],env1++env2,[]) rho
     ; return(map h env1,ans)
     }


-------------------------------------------
-- Create a scheme

generalize:: [Pointer Typ] -> Typ -> FIO Scheme
generalize us t = generalizeRho us (Tau t)
     
generalizeRho:: [Pointer Typ] -> Rho -> FIO Scheme
generalizeRho us t = 
  do { (vs,bad) <- get_tvsRho t
     ; let ok (p,k) = not(elem p us)
           free = filter ok vs
           argKind (_,TyVar nam k) = (nam,k)
     ; let subst = goodNames bad free names
     ; body <- rhoSub noPos (subst,[],[]) t
     ; body2 <- alphaRho body
     ; return (Sch (map argKind subst) body2)}     

-- WARNING !!!, This is nonMonadic so it ignores things down TcTv links
toScheme x = Sch vs (Tau x) where vs = getTyVars x

matchErr loc messages t1 t2 = fail ("\n*** Error, near "++show loc++"\n"++(concat messages))
 
names = (map (:[]) "abcdefghijklmnpqrstuvwxyz") ++ map f names
  where f s = s++"1"

goodNames bad [] names = []
goodNames bad ((p,k):more) (n:names) =
   if elem n bad then goodNames bad ((p,k):more) names
                 else (p,TyVar n k):goodNames bad more names
                 
alphaRho (Rarr s r) = do { s' <- alpha s; r' <- alphaRho r; return(Rarr s' r')}
alphaRho (Tau t) = return(Tau t)                 
                 
alpha:: Scheme -> FIO Scheme
alpha (Sch vs rho) =
  do { (us,bad) <- get_tvsRho rho
     ; let pairs = goodNames bad vs names
           sub = ([],pairs,[])
           argKind (_,TyVar nam k) = (nam,k)
     ; rho2 <- rhoSub noPos sub rho
     ; rho3 <- alphaRho rho2
     ; return(Sch (map argKind pairs) rho3)
     }
-------------------------------------------------
-- operations on Kinds

freshKind = 
  do { n <- fio(nextInteger)
     ; p <- fio(newIORef Nothing)
     ; return(Kvar(n,p)) }
     
get_kvs:: Kind -> FIO [Pointer Kind]
get_kvs t = do { x <- pruneK t; f x }
  where f Star = return []
        f (DomKind s) = return []
        f (Karr x y) =
          do { ts <- get_kvs x 
             ; ss <- get_kvs y
             ; return(union ts ss)}
        f (Kvar (_,ptr)) = return([ptr])   

pruneK :: Kind -> FIO Kind
pruneK (k @ (Kvar(uniq,ref))) =
  do { maybet <- fio(readIORef ref)
     ; case maybet of
         Nothing -> return k
         Just k1 -> do{k2 <- pruneK k1; fio(writeIORef ref (Just k2)); return k2}}
pruneK t = return t

unifyK :: Loc -> [String] -> Kind -> Kind -> FIO ()
unifyK loc message x y = do { x1 <- pruneK x; y1 <- pruneK y; f x1 y1 }
  where f (t1@(Kvar n)) (t2@(Kvar n1)) | n==n1 = return ()
        f (Karr x y) (Karr a b) = do { unifyK loc message x a; unifyK loc message y b }
        f Star Star = return ()
        f (DomKind s) (DomKind t) | s==t = return ()
        f (Kvar x) t = unifyVarK loc message x t
        f t (Kvar x) = unifyVarK loc message x t 
        f s t = matchErr loc (("\nDifferent kinds "++show s++" =/= "++show t): message)  s t

unifyVarK loc message (uniq,r1) t =
  do { vs <- get_kvs t
     ; if (any (==r1) vs) 
          then (matchErr loc ("\nKind occurs check" : message)  (Kvar(uniq,r1)) t)
          else return ()
     ; fio(writeIORef r1 (Just t))
     ; return ()
     }

zonkKind x = do { x1 <- pruneK x; f x1}
  where f (Kvar n) = return (Kvar n)
        f (Karr x y) = do { a <- zonkKind x; b <- zonkKind y; return(Karr a b) }
        f Star = return Star
        f (DomKind b) = return (DomKind b)

---------------------------------------------

domRoot:: Typ -> Maybe Base
domRoot (TyCon c (DomKind b)) = Just b
domRoot (TyTuple (DomKind b) xs) = Just b
domRoot _ = Nothing

tupleT [] = error "Tuple of width 0"
tupleT [t] = t
tupleT ts = TyTuple k ts
  where roots = (map domRoot ts)
        isJust (Just x) = True
        isJust Nothing = False
        unJust (Just x) = x
        k | all isJust roots = DomKind (Tuple (map unJust roots))
          | all (not . isJust) roots = Star
          | True = error ("Mixed Domain and Star in tuple: "++show ts)
       
---------------------------------------------
-- Some common types

arrT x y = TyApp (TyApp tarr x) y       
pairT x y = TyTuple Star [x,y] -- TyApp (TyApp tpair x) y
listT x = TyApp tlist x
setT x = TyApp tset x
stringT = listT tchar
eitherT x y = TyApp (TyApp teither x) y
ioT x = TyApp tio x
maybeT x = TyApp tmaybe x

applyT [] = tunit
applyT [t] = t
applyT (t:s:ts) = applyT (TyApp t s : ts)


tlist    = TyCon "[]"      (Karr Star  Star)
tset     = TyCon "Set"     (Karr Star  Star)
-- tpair    = TyCon "(,)"     (Karr Star (Karr Star Star))
tarr     = TyCon "(->)"    (Karr Star (Karr Star Star))
tmonad   = TyCon "Monad"   (Karr (Karr Star Star) Star)
tbool    = TyCon "Bool"    Star
teither  = TyCon "Either"  (Karr Star (Karr Star Star))
tmaybe   = TyCon "Maybe"   (Karr Star Star)
tinteger = TyCon "Integer" Star
tint     = TyCon "Int"     Star
tdouble  = TyCon "Double"  Star
tchar    = TyCon "Char"    Star
tunit    = TyCon "()"      Star
tio      = TyCon "IO"      (Karr Star Star)

toTyp Int = tint
toType String = stringT
toType Char = tchar
toType Bool = tbool
toType Double = tdouble

ta = TyVar "a" Star
tb = TyVar "b" Star
tc = TyVar "c" Star

failT m a =  a `arrT` (TyApp m a)
failSch m a = Sch [(av,Star)] (Tau (failT m a))
 where (TyVar av k) = a

bindT m a b = (TyApp m a) `arrT` ((a `arrT` (TyApp m b)) `arrT` (TyApp m b))
bindSch m a b = Sch [(av,Star),(bv,Star)] (Tau (bindT m a b))
  where (TyVar av _) = a  
        (TyVar bv _) = b
       
returnT m a = a `arrT` (TyApp m a)
returnSch m a = Sch [(av,Star)] (Tau (returnT m a))
  where (TyVar av k) = a

predefinedTyCon = 
  [("[]",tlist)
 --  ,("(,)",tpair)
  ,("(->)",tarr)
  ,("Monad",tmonad)
  ,("Bool",tbool)
  ,("Either",teither)
  ,("Maybe",tmaybe)
  ,("Integer",tinteger)
  ,("Double",tdouble)
  ,("Char",tchar)
  ,("()",tunit)
  ]
  
preDefinedDataDeclStrings = 
          ["data Bool = False | True"
          ,"data Either a b = Left a | Right b"
          ,"data Maybe a = Nothing | Just a"
          ,"data Monad m = Mon (forall a .a -> m a) (forall a b . m a -> (a -> m b) -> m b)(forall a . [Char] -> m a)"]

