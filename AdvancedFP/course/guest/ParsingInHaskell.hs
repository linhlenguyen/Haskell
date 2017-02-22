module ParsingInHaskell where

import Char(isAlphaNum,isDigit,ord,isLower,isUpper,isAlpha,isSpace)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token(lexeme,whiteSpace,integer)
import Text.ParserCombinators.Parsec.Language(haskell)
import Text.ParserCombinators.Parsec.Combinator(chainl1)

run :: Show a => Parser a -> String -> IO () 
run p input = 
   case (parse p "" input) of 
       Left err -> 
         do{ putStr "parse error at " 
           ; print err } 
       Right x -> print x 

test1 = do { string "A"
           ; char ' '
           ; string "big"
           ; char ' '
           ; string "cat"
           }
           
test2 = do { a <- string "A"
           ; char ' '
           ; b <- string "big"
           ; char ' '
           ; c <- string "cat"
           ; return(a,b,c)
           }           


word s = lexeme haskell (string s)

test3 = do { a <- word "A"
           ; b <- word "big"
           ; c <- word "cat"
           ; return(a,b,c)
           }   

{-
<Sentence>      ::=       <Subject> <Predicate>
<Subject>       ::=       <Pronoun1> | <Pronoun2>
<Pronoun1>      ::=       I | we | you | he | she | it | they
<Noun Phrase>   ::=       <Simple Noun Phrase> | <Article> <Noun Phrase>
<Article>       ::=       a | an | the
<Predicate>     ::=       <Noun> | <Adjective> <Simple Noun Phrase>
<Simple Noun Phrase>    ::=       <Verb> | <Verb> <Object>
<Object>        ::=       <Pronoun2> | <Noun Phrase>
<Pronoun2>      ::=       me | us | you | him | her | it | them
<Noun>  ::=       . . . 
<Verb>  ::=       . . .
-}




sentence = do { subject; verb; predicate}
pronoun1 =  word "I" <|> word "we" <|> word "you" <|>
            word "he" <|> word "she" <|> word "it" <|> word "they"
pronoun2 = word "me" <|> word "us" <|> word "you" <|> word "him" <|>
           word "her" <|> word "it" <|> word "them"
subject  = pronoun1 <|> pronoun2
article  = word "a" <|> word "the"
predicate = do { article; (noun <|> simpleNounPhrase) }
adjective = word "red" <|> word "pretty"
noun = word "cat" <|> word "ball"
simpleNounPhrase =  do { adjective; simpleNounPhrase} <|> return ""
object = pronoun2 <|> nounPhrase
nounPhrase =  simpleNounPhrase <|> do {article; noun}
verb = word "ate" <|> word "hit"

test4 = run sentence "I hit the pretty red cat"

-------------------------------------------------------------

data Variable = Var String
 deriving (Show,Eq)

data Expression
 = Constant Integer                   -- 5
 | Contents Variable                  -- x
 | Minus Expression Expression        -- x - 6
 | Greater Expression Expression      -- 6 > z
 | Times Expression Expression        -- x * y
 deriving (Show,Eq)


parens x = between (char '(') (char ')') x

pVar = lexeme haskell
          (do { c <- lower
              ; cs <- many (satisfy isAlphaNum)
              ; return(Var (c:cs))
              })
              
simpleExp :: Parser Expression
simpleExp =
  (do { n <- integer haskell; return(Constant n)}) <|>
  (do { n <- pVar; return(Contents n)}) <|>
  (parens relation)
  
factor = chainl1 simpleExp
                 (lexeme haskell (char '*')>> return Times) 

summand = chainl1 factor 
                  (lexeme haskell (char '-')>> return Minus)

relation = chainl1 summand
                   (lexeme haskell (char '>') >> return Greater)
  
pExp = relation

test5 =  run pExp "x - 2 > 5"
  
              