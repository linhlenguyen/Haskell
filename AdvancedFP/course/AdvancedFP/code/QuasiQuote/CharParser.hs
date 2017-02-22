{-# Language FlexibleContexts  #-}


module CharParser where

-- These are for defining parsers
import qualified Text.Parsec as P  

{-
(ParsecT(..),runParserT
                  ,satisfy,string,char,many,(<|>)
                  ,(<?>),unexpected)
-}

import Text.Parsec.Prim(getState,getInput)
import Text.Parsec.Combinator
import Text.Parsec.Error(ParseError)

-- This is for operations on simple Data
import Data.Char(digitToInt,isUpper,isLower,isAlpha,isDigit,isSpace)

-- Monad transformers
import Control.Monad.Identity

--------------------------------------------------

many:: MParser a -> MParser [a]
many = P.many

many1:: MParser a -> MParser [a]
many1 = P.many1

string :: String -> MParser String
string = P.string

char:: Char -> MParser Char
char = P.char

(<|>):: MParser a -> MParser a -> MParser a
(<|>) = (P.<|>)

(<?>):: MParser a -> String -> MParser a
(<?>) = (P.<?>)

unexpected :: String -> MParser a
unexpected = P.unexpected 



-- The simplest Parsec parser.

type MParser a =
   P.ParsecT
    String    -- The input is a sequence of Char
    ()        -- The internal state
    Identity  -- The underlying monad
    a         -- the type of the object being parsed

type SrcName = String

-- Extract a computation from the Parser Monad

runMParser
  :: MParser a -> SrcName -> String
     -> Either ParseError a
runMParser parser name tokens =
  runIdentity (P.runParserT parser () name tokens)



-- Skip whitespace before you begin

parse1:: SrcName -> MParser a -> String
         -> Either ParseError a
parse1 file x s = runMParser (whiteSpace >> x) file s



-- Raise the an error if it occurs

parseWithName:: SrcName -> MParser a -> String -> a
parseWithName file x s =
  case parse1 file x s of
   Right(ans) -> ans
   Left message -> error (show message)



-- Parse with a default name for the input

parse2 :: MParser a -> String -> a
parse2 x s = parseWithName "keyboard input" x s



-- Parse and return the internal state

parse3 :: MParser b -> String -> IO b
parse3 p s = putStrLn (show state) >> return object
  where (object,state) =
            parse2 (do { x <- p
                       ; st <- getState
                       ; return(x,st)}) s



-- Parse and return the rest-of-input-not-parsed

parse4 :: MParser t -> String -> (t, String)
parse4 p s =
   parse2 (suffixParser p) s


parse5 file p s = parse1 file p' s
  where p' = do { x <- p; rest <- getInput; return(x,rest)}
  
parse6 p s = 
  case parse1 "input" p s of
     Right(ans) -> return ans
     Left message -> fail (show message)

-- Lift a parser to also return the unparsed input

suffixParser :: MParser t -> MParser (t, String)
suffixParser p = do { x <- p
                  ; rest <- getInput
                  ; return (x,rest)}


-- Does the parser succeed on a prefix of the input 

parses:: MParser t -> String -> Maybe (t, String)
parses p s = 
  case parse1 "parses input" (suffixParser p) s of
    Right (prefix,suffix) -> Just (prefix,suffix)
    Left err -> Nothing
    
succeeds :: MParser t -> String -> Bool
succeeds p s =
  case parses p s of
    Just _ -> True
    Nothing -> False
    
------------------------------------------------------

-- The "satisfy" function specialized to a very useful type.
sat:: (Char -> Bool) -> MParser Char
sat = P.satisfy
 
parens x = between (symbol "(") (symbol ")") x

whiteSpace:: MParser String
whiteSpace = P.many (sat isSpace)

lexeme:: MParser a -> MParser a
lexeme p = do { ans <- p; whiteSpace; return ans}
 
symbol:: String -> MParser String
symbol s = lexeme(P.string s)

int :: MParser Int
int = P.many (sat isDigit) >>= (return . read)



