{-
 - Kendall Stewart
 - CS510 Logic & Prog Lang
 - Exercise 2
 - 10.6.2014
 - 
 - General strategy: use CNF simplifier from slides,
 - apply resolution, then apply simple identities
 - and factoring that use the equivalence tester from Exercise 1.
 -}

module Ex2 where

import SimpleProp
import Ex1 (equal)
import Data.List (nub, find, delete)
import Test.QuickCheck hiding (Prop(..))
import Control.Monad(liftM, liftM2)

{- Begin code from slides -}

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

cnfList :: Prop n -> [[Prop n]]   
cnfList x = processCl [x]  

simplifyCNF :: Eq n => [[Prop n]] -> [[Prop n]]
simplifyCNF [] = []
simplifyCNF (x:xs) 
  | elem TruthP x = simplifyCNF xs
  | conjugatePair x = simplifyCNF xs
  | subsumes xs x = simplifyCNF xs
  | otherwise = x : simplifyCNF xs

-- fixed this one by adding || to elem cases
conjugatePair :: Eq n => [Prop n] -> Bool
conjugatePair [] = False
conjugatePair [x] = False
conjugatePair (LetterP x : xs) = elem (NotP (LetterP x)) xs || conjugatePair xs
conjugatePair (NotP x : xs) = elem x xs || conjugatePair xs
conjugatePair (x:xs) = conjugatePair xs

subsumes :: Eq n => [[Prop n]] -> [Prop n] -> Bool
subsumes [] xs = False
subsumes (ys : yss) xs = all ( `elem` xs) ys || subsumes yss xs 

{- end code from slides -}

-- Apply resolution to CNF formulas
resolution :: Eq n => [[Prop n]] -> [[Prop n]]
resolution [] = []
resolution (xs:xss) = case find (containsNeg xs) xss of
  Just ys -> (removeConjugates.nub) (xs ++ ys) : resolution (delete ys xss)
  Nothing -> nub xs : resolution xss
  where removeConjugates  [] = []
        removeConjugates (x:xs) 
          | NotP x `elem` xs = removeConjugates $ delete (NotP x) xs
          | otherwise        = x : removeConjugates xs
        containsNeg xs = \ys -> or $ map (`elem` ys) (map NotP xs)

-- convert clause list to prop structure
fromDisj :: [[Prop a]] -> Prop a
fromDisj = foldr AndP TruthP . map (foldr OrP AbsurdP)

-- convert to CNF clause list, simplify, appliy resolution, convert back
simpleCNF :: Eq n => Prop n -> Prop n
simpleCNF = fromDisj . resolution . simplifyCNF . cnfList

-- framework for applying transformations
push :: (Prop a -> Prop a) -> Prop a -> Prop a
push f (AndP p q) = AndP (f p) (f q)
push f (OrP  p q) = OrP  (f p) (f q)
push f (ImpliesP p q) = ImpliesP (f p) (f q)
push f (NotP p) = NotP (f p)
push f p = p

applyBottomUp, applyTopDown :: (Prop a -> Prop a) -> Prop a -> Prop a
applyBottomUp transform = transform . push (applyBottomUp transform)
applyTopDown  transform = push (applyTopDown transform) . transform 

-- some transformations:
-- apply simple identities / annihilators
identities :: Ord a => Prop a -> Prop a
identities (AndP p TruthP)               = identities p
identities (AndP p AbsurdP)              = AbsurdP
identities (AndP p q) | equal (NotP p) q = AbsurdP
identities (OrP  p TruthP)               = TruthP
identities (OrP  p AbsurdP)              = identities p
identities (OrP  p q) | equal (NotP p) q = TruthP
identities x = x


-- attempt to factor using evaluation
factor :: Ord a => Prop a -> Prop a
factor (AndP (OrP a b)  (OrP c d))  | equal a c = OrP  a (AndP b d)
factor (OrP  (AndP a b) (AndP c d)) | equal a c = AndP a (OrP b d)
factor x = x

simplify:: Ord n => Prop n -> Prop n
simplify = applyTopDown  factor
         . applyBottomUp identities
         . simpleCNF 


-- testing with quickcheck
instance Arbitrary a => Arbitrary (Prop a) where
  -- coarbitrary = undefined
  arbitrary = oneof [return AbsurdP, return TruthP
                    , liftM LetterP arbitrary
                    , liftM NotP arbitrary
                    , liftM2 AndP arbitrary arbitrary
                    , liftM2 OrP arbitrary arbitrary
                    ]

test = quickCheck (\x -> equal (x::Prop Int) (simplify x))
