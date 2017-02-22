-- ML&PL Exercise 2 -- H Forrest Alexander 
module Ex2 where

import SimpleProp
import qualified LazySmallCheck as SC
import LazySmallCheck
import Test.QuickCheck hiding(Prop(..))
import Control.Monad(liftM, liftM2, replicateM)
import Data.List(nub)

-- CNF code from Lecture2.hs
data Discrim a = Alpha a a | Beta a a | Lit a

discrim :: Prop a -> Discrim (Prop a)
discrim TruthP = Lit TruthP
discrim AbsurdP = Lit AbsurdP
discrim (LetterP s) = Lit (LetterP s)
discrim (AndP x y) = Alpha x y
discrim (OrP x y) = Beta x y
discrim (ImpliesP x y) = Beta (NotP x) y
discrim (NotP (OrP x y)) = Alpha (NotP x)  (NotP y)
discrim (NotP (ImpliesP x y)) = Alpha x (NotP y)
discrim (NotP (AndP x y)) = Beta (NotP x) (NotP y)
discrim (NotP (NotP x)) = discrim x
discrim (NotP TruthP) = Lit AbsurdP
discrim (NotP AbsurdP) = Lit TruthP
discrim (NotP (LetterP s)) = Lit (NotP (LetterP s))


processCl :: [Prop a] -> [[Prop a]]
processCl [] = [[]]
processCl (p : ps) =
      case (discrim p) of
               Lit x -> map (x:) (processCl ps)
               Alpha x y -> processCl (x : ps) ++ processCl (y : ps)
               Beta x y -> processCl (x : y : ps)
                           
cnf5 x = processCl [x]  

cnf6 x = foldr AndP TruthP (map (foldr OrP AbsurdP) (cnf5 x))
------------------------------------------------------------------------------


simplify:: Eq n => Prop n -> Prop n
simplify = undistribute . undistribute . simplifycnf . cnf6


-- Use normal forms. Transform to a normal form, apply simplifications,
-- then transform back to the smallest equivalent formula.

-- Simplify a formula in CNF - only looks at the top level AND and the next level OR
simplifycnf :: Eq n => Prop n -> Prop n
simplifycnf cs = case nub $ simpleConj $ map simplifyDisjunction $ nub $ cnfToList cs of
                      [] -> TruthP
                      xs -> foldr1 AndP xs

simplifyDisjunction :: Eq n => Prop n -> Prop n
simplifyDisjunction ds = case nub $ simpleDis $ disToList ds of
                              [] -> AbsurdP
                              xs -> foldr1 OrP xs

-- Aggregate values of a conjunction or disjunction in a list.
cnfToList (AndP a b) = a : cnfToList b
cnfToList TruthP = []

disToList (OrP a b) = a : disToList b
disToList x = [x]

-- Simplify a proposition by eliminating obvious tautologies or falsehoods

-- Really simple (inelegant, inefficient) algorithm:
-- check if anything is present along with its complement using `elem` repeatedly.
-- do a second pass to see if any Absurd/Truth vals present for Conj/Disj respectively;
-- replace with just this value if there is.

simpleConj :: Eq a => [Prop a] -> [Prop a]
simpleConj = simpleConj1 . simpleConj2
simpleConj1 [] = []
simpleConj1 (TruthP : xs) = simpleConj1 xs
simpleConj1 (NotP x : xs) = if x `elem` xs then [AbsurdP] else (NotP x) : simpleConj1 xs
simpleConj1 (x : xs) = if NotP x `elem` xs then [AbsurdP] else x : simpleConj1 xs
simpleConj2 xs = if AbsurdP `elem` xs then [AbsurdP] else xs


simpleDis :: Eq a => [Prop a] -> [Prop a]
simpleDis = simpleDis2 . simpleDis1
simpleDis1 [] = []
simpleDis1 (AbsurdP : xs) = simpleDis1 xs
simpleDis1 (NotP x : xs) = if x `elem` xs then [TruthP] else (NotP x) : simpleDis1 xs
simpleDis1 (x:xs) = if NotP x `elem` xs then [TruthP] else x : simpleDis1 xs
simpleDis2 xs = if TruthP `elem` xs then [TruthP] else xs


-- More simplification to deal with cases like ( B /\ (B \/ C) /\ (B \/ A) ).
-- If `a` appears alone anywhere in a conjunction, it must be true for a true assignment...
-- so we can replace all instances of `a` in disjunctions with TruthP
-- Also flips the propositions around so a second pass can "undistribute" b into a
undistribute (AndP a b) = AndP (replaceTruth a b) a
undistribute x = x

replaceTruth a (OrP x y) = if a == x then TruthP else replaceTruth a y
replaceTruth a y = if a == y then TruthP else y

-- Sample and print one example simplification
testSimp = do
    (x:_) <- sample' arbitrary
    putStrLn (checkPrint x)

main = replicateM 100 testSimp

-- COMMENTS:
-- From my tests, this seems to work pretty well in terms of simplification,
-- usually resulting in formulae I can't see an obvious way to simplify further
-- (or that are definitely simplest possible, like (B /\ ~A) or (T).
-- However, this algorithm is inefficient, and it hangs briefly
-- on "large" inputs - propositions with more than 20-30 clauses.
-- (The arbitrary instances mostly generates very small inputs,
-- but occasionally some came up that were much larger).

-- ERRORS:
-- I haven't seen any non-equivalent propositions generated,
-- but here are some ways this program can fail to simplify propositions:
-- * End up with a clause like (A /\ A), or ( A /\ (A \/ B) /\ (A \/ C) ).
--   The `undistribute` function above is an attempt to handle this,
--   but I ran out of time to make it work correctly.
-- * End up with a clause like (~A \/ ~B \/ ~C) == ~(A /\ B /\ C) - 
--   the latter does not have the form of a binary tree, but is arguably simpler
--   (requiring fewer logical operators). 
--

------------------------------------------------------------------------------
-- it should always be the case that `eval env x == eval env (simplify x)`
-- Testing code from Lecture2.hs
instance Arbitrary a => Arbitrary (Prop a) where
  -- coarbitrary = undefined
  arbitrary = oneof [return AbsurdP, return TruthP
                    , liftM LetterP arbitrary
                    , liftM NotP arbitrary
                    , liftM2 AndP arbitrary arbitrary
                    , liftM2 OrP arbitrary arbitrary
                    ]
newtypeCons x = cons1 x

instance Serial a => Serial (Prop a) where
 series = newtypeCons LetterP SC.\/ cons1 NotP SC.\/ cons2 OrP SC.\/
          cons2 AndP  SC.\/ cons0 AbsurdP SC.\/ cons0 TruthP SC.\/
          cons2 ImpliesP


instance Serial Name where
  series = cons0 A SC.\/ cons0 B  -- SC.\/ cons0 C
  
instance Arbitrary Name where
  -- coarbitrary = undefined
  arbitrary = oneof [return A, return B, return C]


assigns:: [a] -> [[(a,Bool)]]
assigns [] = []
assigns [n] = [[(n,True)],[(n,False)]]
assigns (x:xs) = map ((x,True):) ys ++ map ((x,False):) ys
  where ys = assigns xs  
  
envs = assigns [A,B, C]
eval env x = value (liftE env) x 

liftE pairs x = case lookup x pairs of
                 Just b -> b
                 other -> False

-- shows the unsimplified and simplified expressions side by side
checkPrint x = let y = simplify x 
                in unwords ["Orig: (", show x, 
                       ")\nSimp: (", show y, 
                       ")\nEqui: (", show $ check2 x y, ")"]

-- returns a Bool                 
check x = and[ (eval e x == eval e (simplify x)) | e <- envs] 
check2 x y = and[ (eval e x == eval e y) | e <- envs] 

-- returns a LazySmallCheck property
check3 x = foldl (*&*) (lift True) [ lift(eval e x == eval e (simplify x)) | e <- envs]

t1 = quickCheck check
t2 = smallCheck 3 check3



