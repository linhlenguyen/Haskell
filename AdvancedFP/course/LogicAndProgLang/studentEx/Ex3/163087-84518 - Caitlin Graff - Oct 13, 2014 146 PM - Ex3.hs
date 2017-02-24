module Tableau where

import SimpleProp

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

--This solver is based on the processCl function. It is similar to a Tableau solver, but instead of keeping a tree in memory and . Evaluation of splitProps performs a discrimination, making the new propisition "tree". During running "Unchecked" propositions are remaining unapplied splitProps terms, while non-literal checked propositions are not stored since they are no longer needed. The downsides of this are that the propisitions before a branching are duplicated, the order of the splitProps applications can't be intelligently chosen, and we can't close off any branches early. We have to check for contradictions after everything has been rended to literals.

check :: (Eq a) => Prop a -> Bool
check p =  not $ elem False $ map hasDirectContradiction $ splitProps [NotP p]

splitProps :: [Prop a] -> [[Prop a]]
splitProps [] = [[]]
splitProps (p : ps) =
  case (discrim p) of
    Lit x -> map (x:) (splitProps ps)
    Beta x y -> splitProps (x : ps) ++ splitProps (y : ps)
    Alpha x y -> splitProps (x : y : ps)

hasDirectContradiction :: (Eq a) => [Prop a] -> Bool
hasDirectContradiction ps = elem True [x == NotP y | x <- ps, y <- ps]
