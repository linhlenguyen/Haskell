import LK
import Text.PrettyPrint.HughesPJ(Doc)
import Control.Monad.State(evalStateT)
import Parser(toFormula, toTerm)
import Formula(Formula)
          
-- Examples from Exercise 17, Coble, page 34
l2 = "(P(a) | (exists x . P(f(x)))) --> (exists y . P(y))"
l3 = "(exists x . (P(x) | Q(x))) --> ((exists y . P(y)) | (exists y . Q(y)))"
-- Example by me. Compare to some formulae with similar structure -- which I claim are absurd:
--  * (forall x . exists y . P(x, y)) --> (exists y . forall x . P(x, y))  -- F
--  * (forall x . exists y . P(x, y)) --> (forall y . exists x . P(x, y))  -- F
--  * (exists x . forall y . P(x, y)) --> (exists y . forall x . P(x, y))  -- F
l4 = "(exists x . forall y . P(x, y)) --> (forall y . exists x . P(x, y))" -- T

-- The proofs
p2  = [impliesR 1, orL 1, exR (toTerm "a") 1, axiom 1 1, exL 1, exR (toTerm "f(?b)") 1, axiom 1 1]
p3  = [impliesR 1,orR 1,exL 1,exR (toTerm "?b") 1,exR (toTerm "?b") 2,orL 1,axiom 1 1,axiom 1 2]
p4  = [impliesR 1, allR 1, exL 1, allL (toTerm "?b") 1, exR (toTerm "?c") 1, axiom 1 1]
p2' = [iR, oL, eR (toTerm "a"), ax, eL, eR (toTerm "f(?b)"), ax]
p3' = [iR, oR, eL, eR (toTerm "?b"), eR (toTerm "?b"), oL, ax, ax]
p4' = [iR, alR, eL, alL (toTerm "?b"), eR (toTerm "?c"), ax]

-- Making printable examples
[f2, f3, f4] = map toFormula [l2, l3, l4]
sequents = map (SequentM [] . (:[])) [f2, f3, f4]
[m2, m3, m4, m2', m3', m4'] = zipWith replayM (cycle sequents) [p2, p3, p4, p2', p3', p4']
[ex2, ex3, ex4, ex2', ex3', ex4'] = zipWith z (cycle sequents) [m2, m3, m4, m2', m3', m4']
  where z s m = evalStateT m ([1..], [s]) :: Maybe Doc
