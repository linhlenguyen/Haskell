{-# LANGUAGE FlexibleInstances #-}
module ReadeStyleParser where

import Data.Char(ord,isDigit,isAlpha,isUpper,isLower,isSpace,isAlphaNum)
import Debug.Trace

-- Parsing ala "Elements of Functional Programming"
-- by Chris Reade.  pp 210 - 220

infixr 3 <|>
infixr 4 <&> 

-- Parsing

data Possible a = Ok a | Fail

data Parser token a = P ([ token ] -> Possible (a, [token]))

parse (P f) x = f x 

-------------------------------------------
-- A first parser

numberP :: Parser Char Int
numberP = P numf
  where numf [] = Fail
        numf cs = let digits = takeWhile isDigit  cs
                      rest = dropWhile isDigit cs
                  in if null digits
                     then Fail
                     else Ok (read digits,rest)


char:: Char -> Parser Char Char
char c = P f
  where f (x:xs) | x==c = Ok(x,xs)
        f _ = Fail

--------------------------------------------------                     
-- Combining parsers

(<|>) :: Parser c a -> Parser c a -> Parser c a
(P p1) <|> (P p2)  =
 P(\ cs -> case p1 cs of
            Fail -> p2 cs
            Ok(a,cs1) ->  Ok(a,cs1))
            
(<&>) :: Parser c a -> Parser c b -> Parser c (a,b)
(P p1) <&> (P p2) = 
  P(\ cs -> 
       case p1 cs of
        Fail -> Fail
        Ok(a,cs1) -> 
          (case p2 cs1 of
            Fail -> Fail
            Ok(b,cs2) -> Ok((a,b),cs2)))   

--------------------------------------------
-- There has to be a better way than using
-- low level case and pattern matching.

sat p = P f
  where f (x:xs) | p x = Ok(x,xs)
        f _ = Fail
        
whitespace = sat isSpace
digit = sat isDigit
alpha = sat isAlpha
upper = sat isUpper
lower = sat isLower
alphanum = sat isAlphaNum

--------------------------------------------
-- Parsers are a Monad

instance Functor (Parser a) where
  fmap f (P h) = P g
     where g xs = case h xs of
                    Fail -> Fail
                    Ok(x,ys) -> Ok(f x,ys)
                    

instance Applicative (Parser a) where
  pure x = P(\ xs -> Ok(x,xs))
  (P f) <*> (P g) = P(h)
    where h xs = case f xs of
                   Fail -> Fail
                   Ok(y,ys) -> case g ys of
                                 Fail -> Fail
                                 Ok(z,zs) -> Ok(y z,zs)

-- note similarity between  (>>=) and (<&>)
instance Monad (Parser a) where
  return x = P( \ xs -> Ok(x,xs) )
  (P f) >>= g = P h
    where h xs = case f xs of
                   Fail -> Fail
                   Ok(a,ys) -> case g a of
                                 (P m) -> m ys
  fail s = (P f) where f xs = Fail
                                 
-- note similarity between  (>>=) and (<&>)
-- in fact we could write (<&>) using (>>=)


ampersand p q = p >>= (\x -> q >>= (\y -> return(x,y)))

instance Show a => Show(Possible (a,[Char])) where
  show Fail = "Parse failure"
  show (Ok (x,xs)) = show x++"\ntrailing\n"++show xs            

----------------------------------------------------------
-- Higher level parsers, sequence and parenthesis, lexemes

many :: Show a => Parser c a -> Parser c [a]
many p = lots <|> none
  where lots = do { x <- p; xs <- many p; return (x:xs)}
        none = return []
        
sepBy p sep = lots <|> none
  where lots = do { x <- p; (separate x) <|> (one x)}
        none = return []
        separate x = do { sep; xs <- sepBy p sep; return(x:xs) }
        one x = return[x]
        
surround left p right = 
  do { left; x <- p; right; return x}
        
lexeme p = do { x <- p; many whitespace; return x}

-----------------------------------------------------------
-- building blocks

ident = lexeme (do { x <- alpha; xs <- many alphanum; return(x:xs)})
num = lexeme numberP
parens p   = surround (lexeme(sat (=='('))) p (lexeme(sat (==')')))
brackets p = surround (lexeme(sat (=='['))) p (lexeme(sat (==']')))
braces p   = surround (lexeme(sat (=='{'))) p (lexeme(sat (=='}')))

------------------------------------------------------------
-- An example

data Exp = Var String
         | Minus Exp Exp
         | Const Int
  deriving Show

simple :: Parser Char Exp
simple = var <|> constant <|> parens expP
  where var = do { x <- ident; return(Var x)}
        constant = do { n <- num; return(Const n)}

expP :: Parser Char Exp
expP = do { xs <- sepBy simple (lexeme (sat (=='-'))); combine xs}
  where combine:: [Exp] -> Parser Char Exp
        combine [] = fail "no exps"
        combine [x] = return x
        combine (x:y:zs) = combine ((Minus x y):zs)
        -- combine xs = return(foldl1 Minus xs)

