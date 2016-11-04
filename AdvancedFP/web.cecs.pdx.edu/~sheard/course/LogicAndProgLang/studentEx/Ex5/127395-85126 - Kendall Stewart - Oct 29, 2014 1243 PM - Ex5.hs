{-
 - Kendall Stewart
 - CS510 Logic & Prog Lang
 - Exercise 5
 - 10/28/2014
 -}

import LK
import Parser
import Print
import Text.PrettyPrint.HughesPJ(Doc)
import Control.Monad.State

-- from Smullyan p. 56, 3rd one down
form1 = toFormula "(exists y. P(y) --> (forall x. P(x)))"

-- explicit
proof1 = [ exR (toTerm "?b") 1
         , impliesR 1
         , allR 1
         , axiom 1 1 
         ]

-- with search
proof1' = [ exR (toTerm "?b") 1 -- eR (toTerm "?b") fails for some reason?
          , iR
          , alR
          , ax 
          ]

-- from Smullyan p. 56, 9th one down
form2 = toFormula $ "(exists x. P(x) & Q(x))" ++ 
                    "--> (exists x. P(x)) & (exists x. Q(x))"

-- explicit
proof2 = [ impliesR 1
         , exL 1
         , andL 1
         , andR 1
         , exR (toTerm "?b") 1
         , axiom 1 1
         , exR (toTerm "?b") 1
         , axiom 2 1
         ]

-- with search
proof2' = [ iR
          , eL
          , aL
          , aR
          , eR (toTerm "?b")
          , ax
          , eR (toTerm "?b")
          , ax ]

-- contractR (from coble pg. 19, not provided in LK.hs)
contractionR i (SequentM hs gs) = do
  phi <- nth gs i
  return [SequentM hs (splice gs i [phi, phi])]

-- from Coble p. 34, 3rd one down 
form3 = toFormula "(exists z. P(z) --> P(a) & P(b))"

-- wouldn't compile without type signature
proof3 :: [SequentM -> StateT ([Int], [SequentM]) Maybe [SequentM]]
proof3 = [ contractionR 1
         , exR (toTerm "a") 1
         , exR (toTerm "b") 2
         , impliesR 2
         , impliesR 1 
         , andR 1
         , axiom 1 1
         , axiom 2 1
         ]

-- check a proof
check formula proof = evalStateT m ([1..], [sequent])
  where sequent = SequentM [] [formula]
        m = replayM sequent proof


