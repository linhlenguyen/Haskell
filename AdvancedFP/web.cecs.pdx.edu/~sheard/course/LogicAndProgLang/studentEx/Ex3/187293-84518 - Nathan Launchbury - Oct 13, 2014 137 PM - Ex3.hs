
import Lecture2 
import Ex1
import Ex2
import SimpleProp

tableau :: Prop Int -> Bool
tableau p = not (or (map check (processT [NotP p])))

processT :: [Prop a] -> [[Prop a]]
processT [] = [[]]
processT (p : ps) =
  case (discrim p) of
    Lit x -> map (x:) (processT ps)
    Alpha x y -> processT (x : y : ps)
    Beta x y -> processT (x : ps) ++ processT (y : ps)


check :: [Prop Int] -> Bool
check []           = True
check (TruthP:ps)  = check ps
check (AbsurdP:ps) = False
check (p:ps)       = contradicts p ps && check ps

contradicts :: Prop Int -> [Prop Int] -> Bool
contradicts p qs = and [contra p q| q <- qs]
--contradicts p qs = and [taut (NotP (p /\ q))| q <- qs]

-- Case by case implementation
contra :: Prop Int -> Prop Int -> Bool
contra (LetterP x)        (LetterP y)        = True
contra (NotP (LetterP x)) (LetterP y)        = x/=y
contra (LetterP x) (NotP (LetterP y))        = x/=y
contra (NotP (LetterP x)) (NotP (LetterP y)) = True
contra p AbsurdP                             = False
contra p TruthP                              = True



-- Example Propositions (also have example1, example2, example3, example4)
propEx1 = (p \/ (q /\ r) ~> ((p \/ q) /\ (p \/ r)))

-- Convenience
p, q, r :: Prop Int
p = LetterP 0
q = LetterP 1
r = LetterP 2



