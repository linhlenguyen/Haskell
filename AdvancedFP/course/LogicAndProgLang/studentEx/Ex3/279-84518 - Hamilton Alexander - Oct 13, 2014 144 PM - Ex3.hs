import SimpleProp
import Data.Set(empty, member, insert)
import Data.List(sortBy)
import Data.Ord(comparing)
import Test.QuickCheck(sample', arbitrary, Arbitrary, oneof)
import Control.Monad(liftM2, liftM)

-- The discriminator function

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

-- Rose tree version of Discrim
data DiscrimL a = AlphaL [a] | BetaL [a] | LitL a deriving Show

-- take advantage of associativity to compact many layers of alpha/beta into a list.
packDiscrim :: Discrim (Prop a) -> DiscrimL (Prop a)
packDiscrim (Alpha x y) = AlphaL $ 
    case (packDiscrim (discrim x), packDiscrim (discrim y)) of
        (AlphaL xs, AlphaL ys)  -> ys ++ xs
        (AlphaL xs, _)          -> y : xs
        (_, AlphaL ys)          -> x : ys
        (_, _)                  -> [x, y]
packDiscrim (Beta x y) = BetaL $ 
    case (packDiscrim (discrim x), packDiscrim (discrim y)) of
        (BetaL xs, BetaL ys)  -> ys ++ xs
        (BetaL xs, _)          -> y : xs
        (_, BetaL ys)          -> x : ys
        (_, _)                  -> [x, y]
packDiscrim (Lit x) = LitL x

discriml = packDiscrim . discrim

type Heuristic a = [Prop a] -> (Prop a, [Prop a])
-- Modified processCl to use a heuristic `h`:
-- this heuristic should select which proposition to "check off" first
-- by picking one of the items in the list `xs` and splitting it out.
-- With a valid heuristic `runGoals` produces a list of lists of propositions
-- which correspond to the paths in the tableau diagram as follows:
-- `goals` is a list of propositions to try to prove false.
-- The result `runGoals h goals` is the list of paths of literals (or their negations)
-- which would disprove the goals but with each literal present as its conjugate.

-- For example, 
--      `processCl [0 /\ 1] = [[0], [1]]` since `~(0 /\ 1)` is true 
--          if either `~0` or `~1` is true.

-- As such if all of the paths in the result contain a conjugate pair,
-- then all of the possible ways the goals could be false would require a contradiction,
-- thereby proving that the goals must be true.


runGoals :: Eq a => Heuristic a -> [Prop a] -> [[Prop a]]
runGoals h goals@[] = [[]]
runGoals h goals    = 
  let (p, ps) = h goals 
   in case discriml p of
        LitL x          -> map (prependOrMarkClosed x) (runGoals h ps)
        AlphaL xs       -> concatMap (runGoals h . (flip prependOrMarkClosed ps)) xs
        BetaL xs        -> runGoals h $ foldr prependOrMarkClosed xs ps

prependOrMarkClosed :: Eq a => Prop a -> [Prop a] -> [Prop a]
prependOrMarkClosed x xs = if not1 x `elem` xs then AbsurdP : x : xs else x : xs 

-- Given a list of paths, return True if every path has a conjugate pair and false otherwise.
-- In this case, no path exists with a satisfying assignment.
-- SimpleProp.hs needs to be modified to derive `Ord` for Prop.
allPathsConjugate :: Ord a => [[Prop a]] -> Bool
allPathsConjugate = and . map hasConjugatePairOrFalsehood


-- Given a path, report True if a conjugate pair or AbsurdP appears anywhere
-- (meaning the path is unsatisfiable).
hasConjugatePairOrFalsehood :: Ord a => [Prop a] -> Bool
hasConjugatePairOrFalsehood xs = go xs empty where
    go [] rs = False
    go (x:xs) rs = x == AbsurdP || not1 x `member` rs || go xs (x `insert` rs)


-- Involute a proposition more completely than just adding a layer of `NotP`
not1 TruthP = AbsurdP
not1 AbsurdP = TruthP
not1 (NotP x) = x
not1 (AndP x y) = OrP(not1 x) (not1 y)
not1 (OrP x y) = AndP (not1 x) (not1 y)
not1 (ImpliesP x y) = AndP x (not1 y)
not1 x = NotP x

-- Heuristic used in processCl: pick the first goal.
pickFirst (x:xs) = (x, xs)

-- A few more heuristics to try
pickLitFirst :: Heuristic a
pickAlphaFirst :: Heuristic a
pickAlphaLongestFirst :: Heuristic a
pickBetaFirst :: Heuristic a
pickBetaLongestFirst :: Heuristic a

pickByCase byCase sortFunc xs = let ls = filter (byCase . discriml) xs 
                                 in pickFirst $ if null ls 
                                        then xs 
                                        else ls ++ filter (not . byCase . discriml) xs

isLitCase (LitL x) = True
isLitCase _ = False
isAlphaCase (AlphaL xs) = True
isAlphaCase _ = False
isBetaCase (BetaL xs) = True
isBetaCase _ = False

pickLitFirst = pickByCase isLitCase id
pickAlphaFirst = pickByCase isAlphaCase id
pickBetaFirst = pickByCase isBetaCase id

pickAlphaLongestFirst = pickByCase isAlphaCase 
                      $ sortBy (comparing $ length . (\(AlphaL xs) -> xs) . discriml)
pickBetaLongestFirst = pickByCase isBetaCase 
                      $ sortBy (comparing $ length . (\(BetaL xs) -> xs) . discriml)


-- Determine whether a proposition is a tautology by the tableau method.
-- This function returns True if the proposition is a tautology and False otherwise
-- paired with a proof structure: a list of paths.
-- If any path is free of contradictions, there is a satisfying assignment
-- for the conjugate of the original formula, meaning the original formula is not a tautology.
-- Return just the portion of the proof until a contradiction is shown.
tableau :: Ord a => Heuristic a -> Prop a -> (Bool, [[Prop a]])
tableau h x = let ps = map (takeWhile (/=TruthP) . reverse . map not1) (runGoals h [x])
               in (allPathsConjugate ps, ps)


-- Some quick tests of arbitrary instances (to see that everything makes sense)
-- Print out an example proof (picking propositions to check off arbitrarily by position)
-- as well as the length of this proof (sum of path lengths) and the length of proofs using other heuristics.
-- These tests unfortunately tend to turn up mostly uninteresting formulae;
-- among these, I've noticed that picking alpha or beta first _usually_ doesn't change the total path length.
-- However, sometimes either alpha or beta first leads to a shorter total path length than picking the first.
-- Also, picking longer alpha or betas first doesn't seem to affect path length among these tests.

-- When arbitrary instances turn up much longer formulae,
-- picking literals first or picking arbitrarily by position finishes much faster than picking alpha or beta first...
-- probably just due to traversing/sorting a huge list.
-- This could probably be improved by using a more appropriate data structure such as a heap-based priority queue,
-- to hold the propositions yet to be checked off and prevent needing to manipulate a list repeatedly.

-- example counts:
--Tableau proof (pick first):        False , 552
--Tableau proof (pick lits first):    False , 556
--Tableau proof (pick alphas first):  False , 383
--Tableau proof (pick long alphas first): False , 383
--Tableau proof (pick betas first):   False , 552
--Tableau proof (pick long betas first):  False , 552

-- another run:
--Tableau proof (pick first):     False , 118
--Tableau proof (pick lits first):    False , 145
--Tableau proof (pick alphas first):  False , 184
--Tableau proof (pick long alphas first): False , 184
--Tableau proof (pick betas first):   False , 118
--Tableau proof (pick long betas first):  False , 118

-- a run of a particularly big input:
--Tableau proof (pick first):       False , 4469
--Tableau proof (pick lits first):  False , 4461
--Tableau proof (pick alphas first): False , 5909       -- this line took much longer (~10m)
--Tableau proof (pick long alphas first): False , 5909  -- this line too much longer (~10m)
--Tableau proof (pick betas first):   False , 4469
--Tableau proof (pick long betas first):  False , 4469


test = do
    xs <- sample' arbitrary
    mapM checkPrint xs

checkPrint :: Prop Int -> IO ()
checkPrint x = do
    putStrLn $ "Test proposition: " ++ show x
    putStrLn $ "Tableau proof (pick first):\t\t" ++ (show $ tableau pickFirst x)
    putStrLn $ "Tableau proof (pick first):\t\t" ++ (showLength $ tableau pickFirst x)
    putStrLn $ "Tableau proof (pick lits first):\t" ++ (showLength $ tableau pickLitFirst x)
    putStrLn $ "Tableau proof (pick alphas first):\t" ++ (showLength $ tableau pickAlphaFirst x)
    putStrLn $ "Tableau proof (pick long alphas first):\t" ++ (showLength $ tableau pickAlphaLongestFirst x)
    putStrLn $ "Tableau proof (pick betas first):\t" ++ (showLength $ tableau pickBetaFirst x)
    putStrLn $ "Tableau proof (pick long betas first):\t" ++ (showLength $ tableau pickBetaLongestFirst x)

showLength (b, p) = unwords [show b, ",", show (sum $ map length p)]

instance Arbitrary a => Arbitrary (Prop a) where
  -- coarbitrary = undefined
  arbitrary = oneof [return AbsurdP, return TruthP
                    , liftM LetterP arbitrary
                    , liftM NotP arbitrary
                    , liftM2 AndP arbitrary arbitrary
                    , liftM2 OrP arbitrary arbitrary
                    ]


main = test
