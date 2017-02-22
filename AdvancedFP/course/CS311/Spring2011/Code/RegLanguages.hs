{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module RegLanguages where

import qualified Data.Set as DS
-- These are for defining parsers
import Text.ParserCombinators.Parsec  
import Text.ParserCombinators.Parsec.Language(javaStyle,haskellStyle)
import Text.ParserCombinators.Parsec.Expr(Operator(..),Assoc(..),buildExpressionParser)
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Prim(getInput)
import Data.Char(isUpper,isAlpha,isDigit)
import Data.List(sortBy)

import qualified Control.Exception as Ex                  

-------------------------------------------------------------------------

printSetSize = 100
starLimit = 4

plistf :: (a -> String) -> String -> [a] -> String -> String -> String
plistf f open xs sep close = open ++ help xs ++ close
  where help [] = ""
        help [x] = f x
        help (x:xs) = f x ++ sep ++ help xs    

comp x y | length x < length y = LT
comp x y | length y < length x = GT
comp x y = compare x y

------------------------------------------------------
newtype Set s = Set (DS.Set s)

instance Show (Set String) where
  show x = showN printSetSize x

showN n (Set xs) = plistf show "{" (take n (sortBy comp (DS.toList xs))) "," "}"


instance Eq s => Eq (Set s) where
  (Set x) == (Set y) = x==y


------------------------------------------------------  

one x = Set(DS.insert [x] DS.empty)
lam:: Ord a => Set [a]
lam = Set(DS.insert [] DS.empty)
empty = Set DS.empty
union (Set x) (Set y) = Set(DS.union x y)
cat (Set xs) (Set ys) = 
   Set (DS.fromList [ x++y | x <- DS.toList xs, y <- DS.toList ys])
starN 0 x = lam
starN 1 x = union lam x
starN n x = union lam (union x (cat x (starN (n-1) x)))
star x = starN starLimit x

----------------------------------------------------
data RegExp a
  = Lambda
  | Empty
  | One a
  | Union (RegExp a) (RegExp a)
  | Cat (RegExp a) (RegExp a)  
  | Star (RegExp a)
  
meaning:: Ord a => Int -> (RegExp a) -> Set [a]
meaning n (One x) = one x
meaning n Lambda = lam
meaning n Empty = empty
meaning n (Union x y) = union (meaning n x) (meaning n y)
meaning n (Cat x y) = cat (meaning n x) (meaning n y)
meaning n (Star x) = starN n (meaning n x)

instance Show a => Show (RegExp a) where
  show (One x) = "(One "++show x++")"
  show Lambda = "lam"
  show Empty = "empty"
  show (Union x y) = "("++show x++"+"++show y++")"
  show (Cat x y) = "("++show x++"."++show y++")"
  show (Star x) = show x++"*"

------------------------------------------------------  
-- Paresers for RegExp

reglang  = makeTokenParser haskellStyle 

lexemE p    = lexeme reglang p
parenS p    = between (symboL "(") (symboL ")") p
symboL      = symbol reglang
ident       = identifier reglang
sym         = symbol reglang
intP:: Parser Int
intP     = fmap fromInteger (integer reglang)
whiteSp     = whiteSpace reglang

-------------------------------------------------------------

catList f [] = Lambda
catList f [x] = f x
catList f (x:xs) = Cat (f x) (catList f xs)

unionList [] = Empty
unionList [x] = x
unionList (x:xs) = Union x (unionList xs)

simpleRE:: Parser (RegExp Char)
simpleRE = empty <|> one <|> more <|> alphanum <|> parenS regexp
  where empty = symboL "{}" >> return Empty
        one = do{ c <- charLiteral reglang; return(One c)}
        more = do { s <- stringLiteral reglang; return(catList One s)}
        alphanum = do { s <- lexemE(satisfy alphanumeric); return (One s)}
        alphanumeric x = isAlpha x || isDigit x
                                     
starRE:: Parser (RegExp Char)
starRE = do { x <- simpleRE; f <- post; return(f x)}
  where post = (symboL "*" >> return Star) <|> (return id)

catRE:: Parser (RegExp Char)
catRE =    (do {ts <- sepBy1 starRE ((symboL ".")<|> return "");return(catList id ts)})
    
regexp:: Parser (RegExp Char)
regexp = do { xs <- sepBy1 catRE (symboL "+"); return(unionList xs)}

data Command 
   = ExpCom (RegExp Char) 
   | StarCom Int 
   | PrintCom Int 
   | QuitCom
   | NullCom
 deriving Show

command :: Parser Command
command =  colon <|> fmap ExpCom regexp <|> (eof >> return NullCom)
  where colon = (sym ":") >> (try star <|> try print <|> try quit)
        star = do { sym "star"; n <- intP; return(StarCom n)}
        print = do { sym "print"; n <- intP; return(PrintCom n)}
        quit = do {sym "q"; return QuitCom}

-------------------------------------------------
-- Running parsers
              
parse2 p s = case parse p "Keyboard input" s of
                Left s -> error (show s)
                Right x -> x

              
observeSuffix x = 
  (do { a <- x; left <- getInput; return(a,take 20 left)})

ps x s = parse2 (observeSuffix x) s

-------------------------------------------------------------
--

main = loop2 (printSetSize,starLimit)
 
loop2 (setSize,starReps) = Ex.catches 
   (do { putStrLn "prompt>"
       ; s <- getLine
       ; case parse (whiteSp >> command) "keyboard input" s of
           Left s -> putStrLn (show s) >> loop2 (setSize,starReps)
           Right QuitCom -> return ()
           Right NullCom -> loop2 (setSize,starReps)
           Right (ExpCom x) -> 
              do { putStrLn(showN setSize (meaning starReps x))
                 ; loop2 (setSize,starReps) }
           Right (PrintCom n) -> 
              putStrLn ("print "++show n) >> loop2 (n,starReps) 
           Right (StarCom n) -> 
              putStrLn ("star "++show n) >> loop2 (setSize,n)            
            
       }) handlers
  where catchError (Ex.ErrorCall s) = putStrLn s >> loop2 (setSize,starReps)
        handlers = [Ex.Handler catchError]
        