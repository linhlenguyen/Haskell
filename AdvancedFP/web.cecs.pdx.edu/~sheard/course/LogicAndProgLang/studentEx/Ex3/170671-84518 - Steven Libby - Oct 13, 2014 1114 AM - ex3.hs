import SimpleProp
import Data.Set hiding(map)

data Discrim a = Alpha a a | Beta a a | Lit a deriving(Show)

discrim                       :: Prop a -> Discrim (Prop a)
discrim TruthP                = Lit TruthP
discrim AbsurdP               = Lit AbsurdP
discrim (LetterP s)           = Lit (LetterP s)
discrim (AndP x y)            = Alpha x y
discrim (OrP x y)             = Beta x y
discrim (ImpliesP x y)        = Beta (NotP x) y
discrim (NotP TruthP)         = Lit AbsurdP
discrim (NotP AbsurdP)        = Lit TruthP
discrim (NotP (LetterP s))    = Lit (NotP (LetterP s))
discrim (NotP (OrP x y))      = Alpha (NotP x)  (NotP y)
discrim (NotP (ImpliesP x y)) = Alpha x (NotP y)
discrim (NotP (AndP x y))     = Beta (NotP x) (NotP y)
discrim (NotP (NotP x))       = discrim x


tableau   :: (Ord a) => Prop a -> Bool
tableau p = not $ superTab [NotP p] empty

--this is simpler than writing insert every time I want to put something into a set
--it's like : for lists
infixr |-
a |- b = insert a b

--simple optimization using sets instead of lists for variables
tab             :: (Ord a) => [Prop a] -> Set (Prop a) -> Bool
tab [] vs       = True
tab (p : ps) vs = 
  case (discrim p) of
    Lit TruthP        -> tab ps vs
    Lit AbsurdP       -> False
    Lit x@(LetterP l) -> if (NotP x) `member` vs then False else (tab ps (x |- vs))
    Lit (NotP x)      -> if x        `member` vs then False else (tab ps ((NotP x) |- vs))
    Alpha x y         -> tab (x : y : ps) vs
    Beta  x y         -> tab (x : ps) vs || tab (y : ps) vs

--less simple optimization that stores every predicate we encounter, so if we
--encounter p and ~p we can stop immediatly
superTab             :: (Ord a) => [Prop a] -> Set (Prop a) -> Bool
superTab [] vs       = True
superTab (p : ps) vs = 
  case (discrim p) of
    Lit TruthP        -> tab ps vs
    Lit AbsurdP       -> False
    Lit x@(LetterP l) -> if check (NotP x) then False else (tab ps (x |- vs))
    Lit (NotP x)      -> if check x        then False else (tab ps ((NotP x) |- vs))
    Alpha x y         -> if check x && check y then False else tab (x : y : ps) (x |- y |- vs)
    Beta  x y         -> (if check x then False else tab (x : ps) (x |- vs)) ||
                         (if check y then False else tab (y : ps) (y |- vs))
 where check TruthP   = True
       check AbsurdP  = False
       check (NotP x) = x `member` vs
       check x        = (NotP x) `member` vs

