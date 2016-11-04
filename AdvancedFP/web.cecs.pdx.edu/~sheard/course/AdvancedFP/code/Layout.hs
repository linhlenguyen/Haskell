-- <PRE>

{-# LANGUAGE FlexibleContexts #-}
module Parser where

import qualified System.IO
import Data.Char(digitToInt,isUpper)
import GHC.Float(double2Float)

-- These are for defining parsers
import Text.Parsec hiding (State)
import Text.Parsec.Expr(Operator(..),Assoc(..),buildExpressionParser)
-- Replaces Text.Parsec.Token
import qualified LayoutToken as Token

-- This is for possible Monads underlying the Parsing Monad
import Control.Monad.State
import Data.Functor.Identity(Identity(..))

-- This is to catch errors when Parsing
import qualified Control.Exception

import Debug.Trace


-- import the Hughes Pretty Printing library qualified
import qualified Text.PrettyPrint.HughesPJ as PP
-- import a few widely used operations without qualification
import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($$),($+$),render)


-----------------------------------------------
-- running parsers

-- Extract a computation from the Parser Monad
runMParser parser parserState name tokens =
  runIdentity (runParserT parser parserState name tokens)

-- Skip whitespace before you begin
parse1 file x = runMParser (whiteSp >> x) initColumns file

-- Raise an Haskell error if a parsing error occurs
parseWithName file x s =
  case parse1 file x s of
   Right(ans) -> ans
   Left message -> error (show message)

-- Parse with a default name for the input
parse2 x s = parseWithName "keyboard input" x s

-- Parse and return the internal state
parse3 p s = putStrLn (show state) >> return object
  where (object,state) = parse2 (do { x <- p; st <- getState; return(x,st)}) s

-- Parse an t-object, return
-- (t,rest-of-input-not-parsed)
parse4 p s =
   parse2 (do { x <- p
              ; rest <- getInput
              ; return (x,rest)}) s

-- Parse a string in an arbitray monad
parseString x s =
  case parse1 s x s of
   Right(ans) -> return ans
   Left message -> fail (show message)

-- Parse a File in the IO monad
parseFile parser file =
    do possible <- Control.Exception.try (readFile file)
       case possible of
         Right contents -> return(parse4 parser contents)
          {-
          case parse1 file parser contents of
              (Right ans,more) -> return (ans,more)
              (Left message,more) -> error(show message)
-}              
         Left err -> error(show (err::IOError))

--------------------------------------------
-- A parser with internal state of a list of columns
-- use (updateState,setState,getState) to access the [Column]

-- for debugging only
traceP p = do { ((c,vs),_) <- getState; ans <- p; ((d,us),_) <- getState
              ; trace ("In  "++show c++"\nOut "++show d) (return ans)}

initColumns = []

type MParser a = ParsecT
                    String                -- The input is a sequence of Char
                    [Column]              -- The internal state for Layout tabs
                    (Identity)            -- The underlying monad is simple
                    a                     -- the type of the object being parsed

-- Based on Parsec's haskellStyle (which we can not use directly since
-- Parsec gives it a too specific type).

lbStyle = Token.LanguageDef
                { Token.commentStart   = "{-"
                , Token.commentEnd     = "-}"
                , Token.commentLine    = "--"
                , Token.nestedComments = True
                , Token.identStart     = lower
                , Token.identLetter    = alphaNum <|> oneOf "_'"
                , Token.opStart        = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , Token.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , Token.caseSensitive  = True
                , Token.reservedOpNames =
                    ["!","?","\\",":",".", "<", "=", "+", "-", "^", "()", "_", "@"]
                , Token.reservedNames  =
                  ["if","then","else","case","of","let","in"]
                }


(haskellTP,Token.LayFun layout) = Token.makeTokenParser lbStyle "{" ";" "}"

lexemE p    = Token.lexeme haskellTP p
arrow       = lexemE(string "->")
larrow      = lexemE(string "<-")
dot         = lexemE(char '.')
parenS p    = between (symboL "(") (symboL ")") p
braceS p    = between (symboL "{") (symboL "}") p
bracketS p  = between (symboL "[") (symboL "]") p
symboL      = Token.symbol haskellTP
natural     = lexemE(number 10 digit)
whiteSp     = Token.whiteSpace haskellTP
idenT       = Token.identifier haskellTP
keyworD     = Token.reserved haskellTP
commA       = Token.comma haskellTP
resOp       = Token.reservedOp haskellTP
opeR        = Token.operator haskellTP
character c = lexemE(char c)


---------------------------------------------------------------

number ::  Integer -> MParser Char -> MParser Integer
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }

signed p = do { f <- sign; n <- p ; return(f n)}
  where sign = (character '-' >> return (* (-1))) <|>
               (character '+' >> return id) <|>
               (return id)

int32:: MParser Int
int32 = do { n <- signed natural; return(fromInteger n)} <?> "<Int Literal>"

float32:: MParser Float
float32 = do { n <- Token.float haskellTP ; return(double2Float n)} <?> "<Float Literal>"

letParser declP expP =
  do { pos <- getPosition   -- This gets the SourcePos
     ; keyworD "let"
     ; ds <- layout declP (keyworD "in")
     ; exp <- expP
     ; return(ds,exp)}

--------------------------------------------------

data Paragraph
  = Simple [String] (Maybe [Paragraph])
 deriving Show
  
wordChar = lower <|> upper <|> oneOf ".;,'?"

word = many1 wordChar
sentence = many (lexemE word)

paragraphP = do { s <- sentence
                ; zs <- (do { symboL "#"
                            ; ps <- layout paragraphP (return ())
                            ; return(Just ps)}) <|> (return Nothing)
                ; return(Simple s zs)}
                  
main = 
  do { (pp,x) <- parseFile paragraphP "paragraphs.txt"
     ; print pp
     ; print x
     }


  
  




-- </PRE>