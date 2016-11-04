{-# LANGUAGE  FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
module FastBDD where

import Control.Monad.Identity
import qualified Control.Monad.State as MS
import qualified Data.Map as DM
import qualified Data.Array as AR
import qualified Data.Set as DS
import Data.List(sortBy,insert)
import Prop
import SetsAsProp
import qualified FiniteSet as FS
import Graphviz hiding (Shape,shape)
import Debug.Trace

--------------------------------------------

data Shape s i = Leaf Bool | Node s i i deriving (Eq,Ord,Show)

data BDD s = 
  BDD{ ptr:: Int 
     , zeroI :: Bool
     , oneI ::  Bool
     , table :: (AR.Array Int (Shape s Int)) }

-----------------------------------------------
-- Order of variables from root to leaves matters
-- How do we enforce order
--   1) Provide only basic BDDs, which are always in order
--   2) Provide limited ways to combine, that always maintain order

emptyArray = AR.array (2,1) []

true  = (BDD 1 False True emptyArray)
false = (BDD 0 False True emptyArray)
var s = (BDD 2 False True (AR.array (0,2) [(0,Leaf False),(1,Leaf True),(2,Node s 0 1)]))
neg (BDD i x y arr) = BDD i (not x) (not y) arr
conj x y = apply (&&) x y
disj x y = apply (||) x y
imply x y = apply imp x y
xor x y = apply xorB x y

-- Primitive operations on BDDs

imp False x =True
imp True x = x

xorB True False = True
xorB False True = True
xorB _ _ = False


isTrue (BDD 1 False True arr) = True
isTrue (BDD 0 True False arr) = True
isTrue _ = False

isFalse (BDD 0 False True arr) = True
isFalse (BDD 1 True False arr) = True
isFalse _ = False

(x,y,z) = (var "x",var "y",var "z")   

instance (Show x,Ord x) => FS.Boolean (BDD x) where
  true = FastBDD.true
  false = FastBDD.false
  isTrue = FastBDD.isTrue
  isFalse = FastBDD.isFalse
  conj = FastBDD.conj
  disj = FastBDD.disj
  neg = FastBDD.neg
  imply = FastBDD.imply


instance (Ord a,Num a,Show a) => Num (BDD a) where
  fromInteger n = var(fromInteger n)
  x + y = disj x y
  x * y = conj x y
  negate x = neg x
  abs x = undefined
  signum x = undefined
  
--------------------------------------------------------------
-- Turning Prop into BDD with many intermediate BDDs

p2b (LetterP s) = var s
p2b (AndP x y) = conj(p2b x) (p2b y)
p2b (OrP x y) = disj(p2b x) (p2b y)
p2b (ImpliesP x y) = imply (p2b x) (p2b y)
p2b (NotP x) = neg(p2b x)
p2b AbsurdP = false
p2b TruthP = true     
         
-----------------------------------------------
-- The approach above creates many intermediate BDD's
-- and repeats computation (perhaps) many times. We can
-- do better by memozing past computations. To do this
-- we will need to store some stuff in Tables (we will
-- need 3 different tables) 

data Quad s = Q Int
                (DM.Map (Shape s Int) Int)  -- index of structure
                (DM.Map Int (Shape s Int))  -- Structure of index i
                (DM.Map (Int,Int) Int)      -- memo table

-- We will thread the tables trough a computation
-- using a state monad.

type M s a = MS.State (Quad s) a

(!) arr i = (AR.!) arr i
-- fetch k tab = (DM.!) tab k
fetch k tab =   
   case DM.lookup k tab of
     Just z -> z
     Nothing -> error ("Bad call to 'fetch' "++show k++" "++show tab)


-- Operations in the M monad

next2 :: Ord s => M s Int
next2 = MS.mapState f (return ())
  where f ((),(Q i m1 m2 m3)) = (i,(Q (i+1) m1 m2 m3))

find2 :: Ord s => Shape s Int -> M s (Maybe Int)
find2 x = MS.mapState f (return ())
  where f ((),q@(Q i tab1 tab2 tab3)) = ( {-# SCC "lookup" #-} (DM.lookup x tab1) , q)
  
fetch2 :: (Ord s,Show s) => Shape s Int -> M s Int
fetch2 x = MS.mapState f (return ())
  where f ((),q@(Q i tab1 tab2 tab3)) = (fetch x tab1,q)  

update2 :: Ord s => Int -> Shape s Int -> M s Int          
update2 n term =  MS.StateT f
  where f (Q i tab1 tab2 tab3) = Identity $
          (n,(Q i ({-# SCC "insert1" #-} (DM.insert term n tab1)) 
                  ({-# SCC "insert2" #-} (DM.insert n term tab2)) tab3))          


shape2:: (Ord s,Show s) => Int -> M s (Shape s Int)
shape2 n = MS.mapState f (return ())
  where f ((),q@(Q i tab1 tab2 tab3)) = (fetch n tab2,q)
        
getTable = MS.mapState f (return ())
  where f ((),q@(Q i tab1 tab2 tab3)) = (tab3,q)
setTable new = MS.mapState f (return ())
  where f ((),q@(Q i tab1 tab2 tab3)) = (tab3,Q i tab1 tab2 new)  
updateTable (x,y) ans = MS.mapState f (return ())
  where f ((),Q i tab1 tab2 tab3) = ((),Q i tab1 tab2 (DM.insert (x,y)ans tab3))

-- The initial state

initTab:: Ord s => (DM.Map (Shape s Int) Int)
initTab = DM.insert (Leaf False) 0 (DM.insert (Leaf True) 1 DM.empty)

initTab2:: Ord s => (DM.Map Int (Shape s Int))
initTab2 = DM.insert 0 (Leaf False) (DM.insert 1 (Leaf True) DM.empty)
 
-- The initial state
stateZero :: Ord s => Quad s
stateZero = (Q 2 tab1 tab2 tab3)
  where tab1 = initTab -- DM.insert (Leaf False) 0 (DM.insert (Leaf True) 1 DM.empty)
        tab2 = initTab2
        tab3 = DM.empty


-- Once a table is created, any entry can be the root of a BDD.
-- So we create a State computation, run it to get the tables.
-- Given an index in the table extract an answer
-- The function "close" does this, and returns a function
-- that can make BDDs from the internal tables.
 
close:: (Ord s,Show s) => M s a -> (a,Int -> BDD s)
close comp = (ptr,f)
  where Identity(ptr,(Q next tab1 tab2 tab3)) = MS.runStateT comp stateZero
        f n = BDD n False True table
           where reachable = zip (DS.toList (reachShape n (fetch n tab2) (DS.fromList [0,1]))) [0 ..]
                 len = (length reachable) - 1
                 rename x = case lookup x reachable of
                              Just n -> n
                 table = AR.array (0,len) (DM.toList(extract tab2 rename n (fetch n tab2) initTab2))
                 reachShape n (Leaf _) ans = DS.insert n ans
                 reachShape n (Node s _ _) ans | DS.member n ans = ans
                 reachShape n (Node s x y) ans = reachShape x (fetch x tab2) 
                                                (reachShape y (fetch y tab2) (DS.insert n ans))

addOne k v map = 
  case DM.lookup k map of
    Just u -> map
    Nothing -> DM.insert k v map
    
extract tab rename n (Leaf x) ans = addOne (rename n) (Leaf x) ans
extract tab rename n (Node s x y) ans = 
     extract tab rename x (fetch x tab) $
     extract tab rename y (fetch y tab) $
       (addOne (rename n) (Node s (rename x) (rename y)) ans)


-- Create a node, if its not already present
makeNode :: (Ord s,Show s) => s -> Int -> Int -> M s Int
makeNode s a b | a==b = return a           -- CASE 1 page 373
makeNode s a b = do {test <- find2 term; findOrCreate test }
  where term = Node s a b 
        findOrCreate (Just n) = return n   -- CASE 2 page 373
        findOrCreate Nothing  =            -- CASE 3 page 373
           do { n <- next2; update2 n term; return n }
        
---------------------------------------------------- 
-- extract the shape of the root of the BDD tree

shape:: BDD s -> Shape s (BDD s)
shape (BDD 0 x y arr) = Leaf x
shape (BDD 1 x y arr) = Leaf y
shape (BDD n x y arr) = fmap g (arr ! n)  where g n = (BDD n x y arr)

-- extract the shape of a subtree inside the BDD
subShape :: BDD s -> Int -> Shape s Int
subShape (BDD _ x y arr) 0 = Leaf x
subShape (BDD _ x y arr) 1 = Leaf y
subShape (BDD _ x y arr) i = arr ! i

reachable (b@(BDD n x y arr)) = reachShape n (subShape b n) (DS.insert 0 (DS.insert 1 DS.empty))
  where reachShape n (Leaf _) ans = DS.insert n ans
        reachShape n (Node s _ _) ans | DS.member n ans = ans
        reachShape n (Node s x y) ans = reachShape x (subShape b x) (reachShape y (subShape b y) (DS.insert n ans))


withSavedTable:: M s a -> M s a
withSavedTable comp = 
  do { save <- getTable
     ; setTable (DM.empty)
     ; ans <- comp
     ; setTable save
     ; return ans }

-- Now to make a BDD we use the tables to memoize all
-- sub BDDs that we make in the process.
     
fixApply:: (Ord s,Show s) => (Bool -> Bool -> Bool) ->
         (Int -> M s(Shape s Int)) ->
         (Int -> M s(Shape s Int)) ->
         ((Int,Int) -> M s Int) ->
         ((Int,Int) -> M s Int)
fixApply oper f1 f2 call (x1,x2) = do { i <- f1 x1; j <- f2 x2; f (i,j)} where
   varLess x (Node y _ _) = x<y
   varLess x (Leaf _) = True
   f(Leaf n,Leaf m) = fetch2 (Leaf (oper n m)) -- Case 1 page 375      
   f(Node x lx hx,Node y ly hy) | x==y =       -- Case 2 page 375
       do { lo <- call (lx,ly)
          ; hi <- call (hx,hy)
          ; makeNode x lo hi } 
   f(Node x lx hx,term) | varLess x term =     -- Case 3 page 375
       do { lo <- call (lx,x2)
          ; hi <- call (hx,x2)
          ; makeNode x lo hi } 
   f(term,Node y ly hy) | varLess y term =     -- Case 4 page 375
       do { lo <- call (x1,ly)
          ; hi <- call (x1,hy)
          ; makeNode y lo hi }               
                  
apply:: (Ord t,Show t) => (Bool -> Bool -> Bool) -> BDD t -> BDD t -> BDD t  
apply oper (x@(BDD i _ _ _)) (y@(BDD j _ _ _)) = make n where
  (n,make) = close (withSavedTable (applyM (return . subShape x) (return . subShape y) oper (i,j)))

applyM fi fj oper (i,j) = 
  do { tab3 <- getTable
     ; case DM.lookup (i,j) tab3 of
         Just n -> return n
         Nothing -> do { ans <- fixApply oper fi fj (applyM fi fj oper) (i,j)
                       ; updateTable (i,j) ans
                       ; return ans }}  
                       
------------------------------------------------------------
-- Are 2 BBDs the same function?
same n m =
  case (shape n,shape m) of 
    (Leaf n,Leaf m) -> n==m
    (Node x lx hx,Node y ly hy) | x==y ->
            same lx ly && same hx hy
    (_,_) -> False   

-- A list of all the satisfying solutions
solution n = 
  case (shape n) of
    (Leaf True) -> [[]]
    (Leaf False) -> []
    (Node x lo hi) -> 
      let xs = solution lo
          ys = solution hi
      in map ((x,False):) xs ++ map ((x,True):) ys 
      
tautology (BDD 0 True _ _) = True
tautology (BDD 1 _ True _) = True
tautology _ = False

unsat (BDD 0 False _ _) = True
unsat (BDD 1 _ False _) = True
unsat _ = False

---------------------------------------------------------------
-- Turning Prop into (M s Int) with no intermediate BDDs
-- Important that the implict tables in the Monad only
-- grow and aren't overwritten.

p2Pair oper x y = 
  do { xi <- p2M x; yi <- p2M y
     ; withSavedTable(applyM shape2 shape2 oper (xi,yi)) }

p2M :: (Ord s,Show s) => Prop s -> M s Int
p2M (LetterP s) = do { t <- p2M TruthP; f <- p2M AbsurdP; makeNode s f t}
p2M AbsurdP = fetch2(Leaf False)
p2M TruthP = fetch2(Leaf True)
p2M (AndP x y) = p2Pair (&&) x y
p2M (OrP x y)  = p2Pair (||) x y
p2M (ImpliesP x y) = p2Pair imp x y
p2M (NotP x) = p2Pair xorB x TruthP

drawBDD:: Prop Int -> IO ()
drawBDD x = pnG(f i) >> return ()
  where (i,f) = close (p2M x)
  
  
class Functor f => Sequence f where
  swap:: Monad m => f(m a) -> m(f a)
instance Sequence [] where
  swap = sequence
instance Sequence Maybe where
  swap Nothing = return Nothing
  swap (Just m) = do { x <- m; return(Just x)}
 
p2F:: (Sequence f,Ord s,Show s) => f (Prop s) -> f (BDD s)
p2F x = fmap makeBdd fints
   where (fints,makeBdd) = close (swap(fmap p2M x))
   


instance Simple s =>  Graph (BDD s) where 
  nodes (BDD p z o tab) = ([(0,"F",Red,Oval),(1,"T",Green,Oval)]++ map f pairs)
    where pairs = AR.assocs tab
          f (k,Node v i j) = (k,simple v,None,Oval)
          f (k,Leaf True) = (k,"T",Green,Oval)
          f (k,Leaf False) = (k,"F",Red,Oval)
  edges (BDD p z o tab) = concat(fmap f (AR.assocs tab))
    where f (k,Node var i j) = [(k,i,"F",Red),(k,j,"T",Green)]
          f (k,Leaf _) = []
    
  
      
----------------------------------------------
-- Class instances for Shape and BDD

instance Functor (Shape s) where
  fmap f (Leaf b) = Leaf b
  fmap f (Node s x y) = Node s (f x) (f y)

showBDD :: Show s => BDD s -> String
showBDD (x@(BDD i fl tr arr)) = unlines (reverse(map f pairs))
  where good = reachable x
        ok (k,a) = DS.member k good
        f (k,a) | k==i = (" -> "++show k++" "++show a)
        f (k,a)        = ("    "++show k++" "++show a)
        pairs = filter ok $
                 -- [(0,Leaf fl),(1,Leaf tr)] ++
                 AR.assocs arr

instance Show s => Show (BDD s) where  show x = showBDD x


---------------------------

subsetB x y = p2b (subset x y)
itemB x y = p2b(item x y)

mem univ x xs = same temp (conj temp (subsetB univ xs))
  where temp = (itemB univ x)

