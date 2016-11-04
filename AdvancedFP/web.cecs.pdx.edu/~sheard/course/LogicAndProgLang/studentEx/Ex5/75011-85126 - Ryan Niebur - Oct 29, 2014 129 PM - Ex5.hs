import Formula
import Tactics
import Print
import Text.PrettyPrint.HughesPJ(Doc,text,char,int,(<>),(<+>),($+$),render)
import Subst
import Parser(toFormula,toTerm)
import Control.Monad
import Term
import Control.Monad.State
import LK

-- EXAMPLE 1

exf1 = toFormula "x --> x|y"
exs1 = (SequentM [] [exf1])

exm1:: Proof Doc
exm1 = (replayM exs1 [impliesR 1, orR 1, axiom 1 1])
ex1 = evalStateT exm1 ([1..],[exs1])

-- works with the "try" rules!

pexm1 = (replayM exs1 [iR, oR, ax])
pex1 = evalStateT pexm1 ([1..],[exs1])

-- EXAMPLE 2

exf2 = toFormula "x&y --> x"
exs2 = (SequentM [] [exf2])

exm2:: Proof Doc
exm2 = (replayM exs2 [impliesR 1, andL 1, axiom 1 1])
ex2 = evalStateT exm2 ([1..],[exs2])

-- works with the "try" rules!

pexm2 = (replayM exs2 [iR, aL, ax])
pex2 = evalStateT pexm2 ([1..],[exs2])

-- EXAMPLE 3

exf3 = toFormula "(forall x. P(x)) --> exists x. P(x)"
exs3 = (SequentM [] [exf3])

exm3:: Proof Doc
exm3 = (replayM exs3 [impliesR 1, exR (toTerm "q") 1, allL (toTerm "q") 1, axiom 1 1])
ex3 = evalStateT exm3 ([1..],[exs3])

-- works with the "try" rules!

pexm3 = (replayM exs3 [iR, eR (toTerm "q"), alL (toTerm "q"), ax])
pex3 = evalStateT pexm3 ([1..],[exs3])

