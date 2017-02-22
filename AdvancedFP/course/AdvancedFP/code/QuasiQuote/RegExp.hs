{-# LANGUAGE  FlexibleInstances  #-}
module RegExp where

-- import the Hughes library qualified
import qualified Language.Haskell.TH.PprLib as PP
import Language.Haskell.TH.PprLib(Doc,text,int,(<>),(<+>),($$),($+$))


-- These are for defining parsers
import CharParser                        -- simple String parsers
import Text.Parsec.Combinator(sepBy1)    -- some Parsec combinators

-- For interface to Template Haskell and MiniHaskell parsers in escapes
import MiniHaskell
import qualified Language.Haskell.TH as T
import Language.Haskell.TH.Syntax(Lift,lift)

import Language.Haskell.TH.Quote

-- This is for operations on simple Data
import Data.Char(digitToInt,isUpper,isLower,isAlpha,isDigit,isSpace)

------------------------------------------

-- Consider the regular expressions over alphabetic characters.
-- represented by the datatype below. A regular expression is
-- meant to denote a set of strings over the characters
-- ['a' .. 'z']  and ['A' .. 'Z'], if One is only applied
-- to alphabetic characters.

data (RegExp esc)
  = Epsilon                        -- the empty string {""}
  | Empty                          -- the empty set    {}
  | One Char                       -- a singleton set {a}
  | Union (RegExp esc) (RegExp esc)            -- union of two (RegExp esc)
  | Cat (RegExp esc) (RegExp esc)              -- concatenation
  | Star (RegExp esc)                    -- kleene closure
  | Escape (Esc esc)                     -- $x, $(exp), @x, @(exp)
    deriving Show

data Esc esc = Dollar esc | AtSign esc
  deriving Show
  
-- A regular expression string to be parsed should contain only alpha
-- characters "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
-- meta characters "()+*#%" white space, and Escapes back into Haskell
-- "(" and ")" are used for grouping, "+" for union, "*" is used
-- as postfix kleene closure, "#" is epsilon the empty string,
-- and "%" is the emptyset. Concatenation is indicated by juxtaposition.
-- "*" binds with bhighest precedence, then concatenation, then "+"
-- with lowest precedence.  Note that concatenation and union
-- are associative.

-- some examples

s1 = "a b+c* (#+ w)"
r1 = Union (Cat (One 'a') (One 'b'))
           (Cat (Star (One 'c'))
                (Union Epsilon (One 'w')))

s2 = "$ a"
r2 = Cat Empty (One 'a')

pp4 = 50
-- (4) write a parser for regular expressions over
-- alphabetic characters. We will use layered parsers
-- structured by precedence rules. Simple (one token)
-- parsers, star parsers (the ones with highest precedence)
-- concatenation parsers indicated by juxtaposition (the next
-- highest parsers) and finally, union parsers,
-- (lowest precedence). 50 points


catListRE f [] = Epsilon
catListRE f [x] = f x
catListRE f (x:xs) = Cat (f x) (catListRE f xs)

unionListRE [] = Empty
unionListRE [x] = x
unionListRE (x:xs) = Union x (unionListRE xs)

escape :: MParser esc -> (String -> esc) -> MParser (RegExp esc)
escape expP g = dollar <|> atSign
  where dollar = do { symbol "$"; simple Dollar <|> complex Dollar}
        simple f = do { x <- idString; return(Escape (f (g x)))}
        complex f = do { x <- parens expP; return(Escape (f x))}
        atSign = do { symbol "@"; simple AtSign <|> complex AtSign }

simple f = do { x <- nameP; return(Escape (f x))}


simpleRE:: MParser esc -> (String -> esc) -> MParser (RegExp esc)
simpleRE expP g = empty <|> epsilon <|> one <|> 
                  (escape expP g) <|> parens (regexp expP g)
  where empty = symbol "%" >> return Empty
        epsilon = symbol "#" >> return Epsilon
        one = do{ c <- lexeme (sat isAlpha) ; return(One c)}
        
                                     
starRE:: MParser esc -> (String -> esc) -> MParser (RegExp esc)
starRE expP g = do { x <- simpleRE expP g; f <- post; return(f x)}
  where post = (symbol "*" >> return Star) <|> (return id)

catRE:: MParser esc -> (String -> esc) -> MParser (RegExp esc)
catRE expP g = (do {ts <- many (starRE expP g); return(catListRE id ts)})
    
regexp:: MParser esc -> (String -> esc) -> MParser (RegExp esc)
regexp expP g = do { xs <- sepBy1 (catRE expP g) (symbol "+"); return(unionListRE xs)}

expQuoter s = liftRegExp(parse2 (regexp expP stringToExp)s)
patQuoter s = liftREpat(parse2 (regexp patP stringToPat) s)

r = QuasiQuoter expQuoter patQuoter undefined (\ x -> return[])
-------------------------------------------------------------
-- Pretty Printing
-------------------------------------------------------------

pp5 = 30
-- (5) Write a pretty printer for RegExp. Make some effort to
--    (a) Make pretty printing followed by parsing the identity
--    (b) Not to print extra parentheses that are unneeded
--        because of the precedence and associativity rules.
-- 30 points


ppRE:: (RegExp T.Exp) -> Doc
ppRE (One x) = text [x]
ppRE Epsilon = text "#"
ppRE Empty =  text "%"
ppRE (w@(Union x y)) = PP.parens (PP.cat (PP.punctuate (text "+") (map ppRE (us w))))
   where us (Union x y) = us x++ us y
         us x = [x]
ppRE (w@(Cat x y)) = PP.cat (map ppRE (cats w))
  where cats (Cat x y) = cats x ++ cats y
        cats x = [x]
ppRE (Star (x@(Cat _ _))) = PP.parens (ppRE x) <> text "*"       
ppRE (Star x) = ppRE x <> text "*"
ppRE (Escape (Dollar x)) = text "$" <> T.ppr x
ppRE (Escape (AtSign x)) = text "@" <> T.ppr x
  

pp6 = 5
-- (6) Use ppRE to make (RegExp Char) an instance of
-- the Show class. 5 points

-- instance Show RegExp where
--  show x = show(ppRE x)
  
  
  
------------------------------------------------------
-- The RegExp datatype needs to be lifted into Template Haskell Exp
-- in order to make quasiquotations work.
-- We define liftRegExp, and we define
-- TH.Lift instance of RegExp.

liftList :: (t -> T.ExpQ) -> [t] -> T.ExpQ
liftList f [] =     [e| []                        |]
liftList f (x:xs) = [e| $(f x) : $(liftList f xs) |]


liftRegExp :: RegExp T.Exp -> T.Q T.Exp
liftRegExp Epsilon = [e| Epsilon |]
liftRegExp Empty = [e| Empty |]
liftRegExp (One x) = [e| One $(lift x) |]
liftRegExp (Union x y) = [e| Union $(liftRegExp x) $(liftRegExp y) |]
liftRegExp (Cat x y) =  [e| Cat $(liftRegExp x) $(liftRegExp y) |]
liftRegExp (Star x) =  [e| Star $(liftRegExp x) |]
liftRegExp (Escape (Dollar e)) = return e 
liftRegExp (Escape (AtSign x)) = [e| catListRE id $(return x) |]


liftREpat :: (RegExp T.Pat) -> T.Q T.Pat
liftREpat Epsilon = [p| Epsilon |]
liftREpat Empty = [p| Empty |]
liftREpat (One x) = [p| One $(return(T.LitP (T.CharL x))) |]
liftREpat (Union x y) = [p| Union $(liftREpat x) $(liftREpat y) |]
liftREpat (Cat x y) =  [p| Cat $(liftREpat x) $(liftREpat y) |]
liftREpat (Star x) =  [p| Star $(liftREpat x) |]
liftREpat (Escape (Dollar p)) = return p
-- liftREpat (Escape (AtSign x)) = [p| catListRE id $(return x) |]



instance Lift (RegExp T.Exp) where
  lift x = liftRegExp x

   
