import Formula
import Subst
import Term
import Control.Monad.State
import Data.List((\\))
import Blocks
import Print
import Parser
import Tableau

-- debugging function, ignore
ptree (Direct x y) = show "direct->" ++ show x ++ "\n" ++ ptree y
ptree (Branch x y) = show "branch->" ++ ptree x ++ ptree y
ptree (Leaf)       = show "leaf\n"
ptree (Closed _ _) = show "closed"

-- should be true
t1 = toFormula "(forall x. O(x,x)) --> (forall x. exists y. O(x,y))"
t2 = toFormula "(forall x. O(x,x)) --> (forall x. forall y. O(x,x) | O(y,y))"
t3 = toFormula "(forall x. O(x,x)) --> (forall x. forall y. O(x,x) & O(y,y))"
t4 = toFormula "(forall x. P(x) --> exists x. P(x))"
t5 = toFormula "(exists x. P(x) --> exists y. P(y))"

-- can't get this to parse correctly:
t6 = toFormula "(not exists y. P(y) --> (forall y. exists x. P(x) --> P(y)))"

-- should be false
f1 = toFormula "(forall y. forall x. P(x) --> P(y))"
f2 = toFormula "(not O(x,y)) --> (forall x. forall y. O(x,x) & O(y,y))"
f3 = toFormula "(forall x. O(x,x)) --> (forall x. forall y. not O(x,x) & not O(y,y))"

-- try2 Formula : [br1, br2, ...], Int
allBranches frm = branches where
    (branches, _) = try2 frm

-- someConj br1_1 br1_2, for each branch returned from try2
tryBranches (x:xs) = (someConj (head x) (x)) : tryBranches xs
tryBranches [] = []

-- fromJust above result, or pattern match each someConj result
-- if all branches are not nothing, True, otherwise False

branchClosed (Nothing) = False
branchClosed (Just a)  = True

-- This should work:
try3 :: FormulaS -> Bool
try3 f = all id ls where
    ls = map branchClosed res
    res = tryBranches allb
    allb = allBranches f
