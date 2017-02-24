--Steven Libby
--Logic and Programming Languages
--Exercise 5

import LK
import Text.PrettyPrint.HughesPJ(Doc)
import Parser(toFormula,toTerm)
import Control.Monad.State

--first example is the one in the assignment
f2 = toFormula "(forall x. P(x)) --> (forall y. P(F(y)))"
s2 = (SequentM [] [f2])
m2 :: Proof Doc
m2 = (replayM s2 [impliesR 1,allR 1,allL (toTerm "F(?b)") 1,axiom 1 1])
ex2 = evalStateT m2 ([1..],[s2])

--the second example is from page 51 of "First Order Logic"
--They said this wasn't a tautology, I don't believe them.
--
--proof:
--turn the arrow into a sequent
--pick a value for x (like ?b)
--pick a value for y (?b again)
--Now we have P(?b) & Q(?b) |- P(?b), so we're done
f3 = toFormula "(forall x. P(x) & Q(x)) --> (forall y. P(y))"
s3 = (SequentM [] [f3])
m3 :: Proof Doc
m3 = (replayM s3 [impliesR 1,allR 1,allL (toTerm "?b") 1,andL 1,axiom 1 1])
ex3 = evalStateT m3 ([1..],[s3])

--the third and fourth examples are proofs of DeMorgan's laws for quantified variables.
--
--proof:
--turn the arrow into a sequent
--bick a value for x (?b sounds like a good plan)
--eliminate the negation by moving it to the left
--pick a value for y (I'll go with ?b)
--Now we reintroduce the negation, since the inside of the universal was negated.
--Now we have ~P(?b) |- ~P(?b)
--And we're done
f4 = toFormula "(forall x. not P(x)) --> (not (exists y. P(y)))"
s4 = (SequentM [] [f4])
m4 :: Proof Doc
m4 = (replayM s4 [impliesR 1,allL (toTerm "?b") 1,notR 1,exL 1,notL 3,axiom 1 1])
ex4 = evalStateT m4 ([1..],[s4])

--proof: same as above, but swap existential and universal.
f5 = toFormula "(exists x. not P(x)) --> (not (forall y. P(y)))"
s5 = (SequentM [] [f5])
m5 :: Proof Doc
m5 = (replayM s5 [impliesR 1,exL 1,notR 1,allL (toTerm "?b") 1,notL 2,axiom 1 1])
ex5 = evalStateT m5 ([1..],[s5])
