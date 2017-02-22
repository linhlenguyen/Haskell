{-# LANGUAGE GADTs, FlexibleContexts #-}

module TrivialParser where

-- These are for defining parsers
import Text.Parsec hiding (State,Column)
import Text.Parsec.Expr(Operator(..),Assoc(..),buildExpressionParser)
import Text.Parsec.Prim(getInput)
import Text.Parsec.Token hiding (natural)

-- This is for possible Monads underlying the Parsing Monad
import Control.Monad.State
import Data.Functor.Identity(Identity(..))

-- This is to catch errors when Parsing
import qualified Control.Exception

-- This is for operations on simple Data
import Data.Char(digitToInt,isUpper)
import GHC.Float(double2Float)    

sat x = satisfy (==x)
--------------------------------------------------

type MParser a = 
   ParsecT
    String    -- The input is a sequence of Char
    ()        -- The internal state
    Identity  -- The underlying monad
    a         -- the type of the object being parsed

                    
oneof :: [Char] -> MParser Char
oneof = oneOf
                    
myStyle = 
  LanguageDef
  { commentStart   = "{-"
  , commentEnd     = "-}"
  , commentLine    = "--"
  , nestedComments = True
  , identStart     = lower
  , identLetter    = alphaNum <|> char '_' <|> char '\''
  , opStart        = oneof ":!#$%&*+./<=>?@\\^|-~"
  , opLetter       = oneof ":!#$%&*+./<=>?@\\^|-~"
  , caseSensitive  = True
  , reservedOpNames =
    ["<", "=", "+", "-", "*"]
  , reservedNames  = 
    ["if","then","else", "while", "begin", "end"]
  }                    

myTP = makeTokenParser myStyle

lexemE p    = lexeme myTP p
parenS p    = between (symboL "(") (symboL ")") p
braceS p    = between (symboL "{") (symboL "}") p
bracketS p  = between (symboL "[") (symboL "]") p
symboL      = symbol myTP
whiteSp     = whiteSpace myTP
idenT      = identifier myTP
keyworD    = reserved myTP
commA       = comma myTP
resOp       = reservedOp myTP
opeR       = operator myTP

natural     = lexemE(number 10 digit)
arrow       = lexemE(string "->")
larrow      = lexemE(string "<-")
dot         = lexemE(char '.')
character c = lexemE(char c)

number ::  Integer -> MParser Char -> MParser Integer
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl acc 0 digits
              acc x d = base*x + toInteger (digitToInt d)
        ; seq n (return n)
        }
        
signed p = do { f <- sign; n <- p ; return(f n)}
  where sign = (character '-' >> return (* (-1))) <|> 
               (character '+' >> return id) <|> 
               (return id)

exactly s   = do { t <- idenT; if s==t then return s else unexpected("Not the exact name: "++show s)}

-- 'fail' is for recoverable parsing errors
-- 'fatal' is for ones that are not recoverable.
fatal s = unexpected s

-----------------------------------------------
-- running parsers

-- Extract a computation from the Parser Monad
runMParser parser name tokens =
  runIdentity (runParserT parser () name tokens) 

-- Skip whitespace before you begin
parse1 file x s = runMParser (whiteSp >> x) file s 

-- Raise the an error if it occurs
parseWithName file x s =
  case parse1 file x s of
   Right(ans) -> ans
   Left message -> error (show message)

-- Parse with a default name for the input   
parse2 x s = parseWithName "keyboard input" x s   

-- Parse and return the internal state
parse3 p s = putStrLn (show state) >> return object
  where (object,state) = 
            parse2 (do { x <- p
                       ; st <- getState
                       ; return(x,st)}) s

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
         Right contents -> 
            case parse1 file parser contents of
              Right ans -> return ans
              Left message -> error(show message)
         Left err -> error(show (err::IOError))

---------------------------------------------------
-- Parsers for literal (Prim) types.

bool = ((symboL "True" >> return True) <|> 
        (symboL "False" >> return False)) <?> "<Boolean Literal>"
          
int32:: MParser Int
int32 = do { n <- signed natural; return(fromInteger n)} <?> "<Int Literal>"


float32:: MParser Float
float32 = do { n <- float myTP ; return(double2Float n)} <?> "<Float Literal>"
