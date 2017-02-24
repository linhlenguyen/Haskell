{-# LANGUAGE GADTs #-}

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

--------------------------------------------------

type MParser a = 
   ParsecT
    String    -- The input is a sequence of Char
    ()        -- The internal state
    (StateT (String -> Bool) Identity)  -- The underlying monad
    a         -- the type of the object being parsed

addProcedure:: String -> MParser ()
addProcedure s = 
      lift (withStateT (extend s True) 
                       (return ()))
  where extend:: Eq a => a -> b -> (a -> b) -> (a -> b)
        extend x y f = 
         \ s -> if (s==x) then y else f s
         
testProcedure:: String -> MParser Bool
testProcedure s = do { f <- get; return(f s)}

                    
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
    ["if","then","else", "while","do","call","begin", "end"]
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
-- Note the underlying monad is 
-- (StateT (String -> Bool) Identity)
runMParser parser name tokens =
  runIdentity 
    (runStateT 
       (runParserT parser () name tokens) 
       (const False))


-- Skip whitespace before you begin
parse1 file x s = runMParser (whiteSp >> x) file s 

-- Raise the an error if it occurs
parseWithName file x s =
  case parse1 file x s of
   (Right ans,f) -> ans   -- f is the final (String -> Bool) state
   (Left message,f) -> error (show message)

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
   (Right ans,f) -> return ans
   (Left message,f) -> fail (show message)   

-- Parse a File in the IO monad
parseFile parser file =
    do possible <- Control.Exception.try (readFile file)
       case possible of
         Right contents -> 
            case parse1 file parser contents of
              (Right ans,f) -> return ans
              (Left message,f) -> error(show message)
         Left err -> error(show (err::IOError))

---------------------------------------------------
-- Parsers for literal (Prim) types.
          
int32:: MParser Int
int32 = do { n <- signed natural; return(fromInteger n)} <?> "<Int Literal>"

-------------------------------------------------------------
-- Here we build a parser for an small imperative language

type Name = String
type Op = String

data Exp
  = Var Name 
  | Int Int
  | Bool Bool
  | Oper Exp Op Exp
 deriving Show
         

data Stmt
  = Assign Name Exp
  | While Exp Stmt
  | If Exp Stmt Stmt
  | Call Name [Exp]
  | Begin [Decl] [Stmt]
 deriving Show
 
data Decl
  = Val Name Exp
  | Fun Name [Name] Stmt
 deriving Show 

--------------------------------------------
-- simple expressions

simpleP:: MParser Exp
simpleP = bool <|> var <|> int <|> parenS expP
  where var = fmap Var idenT
        int = do { n <- int32; return(Int n)}
        bool = (symboL "True" >> return (Bool True)) <|> 
               (symboL "False" >> return (Bool False))

liftOp oper x y = Oper x oper y            

-- A sequence of simple separated by "*"
factor = chainl1 simpleP mulop

mulop = (resOp "*" >> return (liftOp "*")) 

-- A seqence of factor separated by "+" or "-"
term = chainl1 factor addop 

addop = (resOp "+" >> return (liftOp "+")) <|> 
        (resOp "-" >> return (liftOp "-"))

-- Expressions with different precedence levels
expP:: MParser Exp
expP = chainl1 term compareop

compareop = (resOp "<" >> return (liftOp "<")) <|> 
            (resOp "=" >> return (liftOp "=")) 

-- some thing to test        
t1 = parse4 expP "4 + x * 9 = True"    

--------------------------------------------
-- statements

assignP = 
  do { x <- idenT
     ; symboL ":="
     ; e <- expP
     ; return (Assign x e)}

whileP = 
  do { keyworD "while"
     ; tst <- expP
     ; keyworD "do"
     ; s <- stmtP 
     ; return (While tst s )}

ifP = 
  do { keyworD "if"
     ; tst <- expP
     ; keyworD "then"
     ; s <- stmtP
     ; keyworD "else"
     ; s2 <- stmtP
     ; return (If tst s s2)}  
     
callP = 
  do { keyworD "call"
     ; f <- idenT
     ; b <- testProcedure f
     ; if b then return () else (unexpected ("undefined procedure call: "++f))
     ; xs <- parenS(sepBy expP commA)
     ; return (Call f xs)}  
     
blockP =
  do { keyworD "begin"
     ; xs <- sepBy (fmap (Left) declP <|>
                    fmap (Right) stmtP) 
                   (symboL ";")
     ; keyworD "end"
     ; return(split [] [] xs)}

split ds ss [] = Begin ds ss
split ds [] (Left d : more) = 
   split (ds ++ [d]) [] more
split ds ss (Left d : more) = 
   Begin ds 
        ( ss ++ 
          [split [] [] (Left d : more)])
split ds ss (Right s : more) = 
   split ds (ss ++[s]) more
     
stmtP = whileP <|> ifP <|> callP <|> blockP <|> assignP

-------------------------------------------
-- declarations

declP = valP <|> funP
  where valP = do { keyworD "val"
                  ; x <- idenT
                  ; symboL "="
                  ; e <- expP                 
                  ; return(Val x e)}
        funP = do { keyworD "fun"
                  ; x <- idenT         
                  ; addProcedure x
                  ; args <- parenS(sepBy idenT commA)
                  ; symboL "="
                  ; body <- stmtP
                  ; return(Fun x args body)}
                  
t2 = parse2 stmtP "begin val x = 5;  fun f (x,y) = y := 4; call f(4,5) end"               
     
     
