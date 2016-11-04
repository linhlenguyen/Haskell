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

-- optimization: do not add if it is there
consOnce :: (Eq a) => Prop a -> [Prop a] -> [Prop a]
consOnce x xs = if (x `elem` xs) then (xs) else (x:xs)

processCl :: (Eq a) => [Prop a] -> [[Prop a]]
processCl [] = [[]]
processCl (p : ps) =
  case (discrim p) of
    Lit x -> map (x:) (processCl ps)
    Alpha x y -> processCl (consOnce x ps) ++ processCl (consOnce y ps)
    Beta x y -> processCl (consOnce x (consOnce y ps))

-- this is a recurse function to check each list
findConflicts [] xs = False
findConflicts (p : ps) xs = ((NotP p) `elem` xs) || findConflicts (ps) (xs)
isClosed x = findConflicts x x

-- this is a recursive function to check the list
allTrue [] = True
allTrue (p : ps) = p && allTrue (ps)

-- this checks that all of the branches are closed
prove e = allTrue (map isClosed (processCl [e]))

-- for testing
p = LetterP "p"
q = LetterP "q"
expr = ImpliesP (p /\ q) (q /\ p)

