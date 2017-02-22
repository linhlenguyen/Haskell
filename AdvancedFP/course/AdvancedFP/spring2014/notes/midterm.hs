{-# Language DeriveFunctor #-}

module Midterm where

-- Be sure and enter your name and email below
-- Name:
-- Email: 

-- This is a "Mark Jones" style of exam. That is, there are
-- many more questions than anyone is expected to answer in the
-- given two hours (to which you are honor bound to respect).
-- Answer as many as you can. Choose questions where you can
-- demonstrate your expertise. You don't have to finish every
-- part of a question to get credit. This file should load
-- into GHCi. To finish the exam you should replace "undefined"
-- and "Undefined" with real working code. I expect your completed
-- exam to load into GHCi as well. Please comment out code
-- that keeps it from laoding.

-----------------------------------------------------------
-- All the imports you should need
-----------------------------------------------------------

-- import the Hughes library qualified 
import qualified Text.PrettyPrint.HughesPJ as PP
-- import a few widely used operations without qualification
import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($$),($+$),render) 

-- These are for defining parsers
import Text.Parsec(ParsecT(..),runParserT
                  ,many,many1,satisfy,string,(<|>),chainl1
                  ,(<?>),unexpected)  
import Text.Parsec.Prim(getState,getInput)
-- This is for possible Monads underlying the Parsing Monad
import Data.Functor.Identity(Identity(..))

-- This is for operations on simple Data
import Data.Char(digitToInt,isUpper,isLower,isAlpha,isDigit,isSpace)

import Control.Monad.Reader

-- Arrows
import Control.Arrow( Arrow, ArrowChoice, ArrowLoop, Kleisli(..)
                    , arr, first, second, (***), (&&&)
                    , left, right, (+++), (|||)
                    , loop )
import Control.Category(Category, (>>>), (.), id)
import Control.Monad(liftM)
import Prelude hiding((.), id)

------------------------------------------------------
-- Definitions used by questions below.
-- You have seen all these in both class and homework.
-- There are no surprises here. They do exactly what
-- you expect them to do.
------------------------------------------------------

data Undefined

data Exp = Add Exp Exp
         | Mult Exp Exp
         | Int Integer
         | Let String Exp Exp
         | Var String

-- The simplest Parsec parser.

type MParser a = 
   ParsecT
    String    -- The input is a sequence of Char
    ()        -- The internal state
    Identity  -- The underlying monad
    a         -- the type of the object being parsed

-- The "satisfy" function specialized to a very useful type.

sat:: (Char -> Bool) -> MParser Char
sat = satisfy

-- Extract a computation from the Parser Monad
runMParser parser name tokens =
  runIdentity (runParserT parser () name tokens) 

-- Skip whitespace before you begin
parse1 file x s = runMParser (whiteSpace >> x) file s 

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

-----------------------------------------
-- Algebras and their duals co-Algebras

data Algebra f c = Algebra (f c -> c)

data CoAlgebra f c = CoAlgebra {unCoAlgebra:: c -> f c }

--------------------------------------------
-- Initial and Final Algebras

data Initial f = Init (f (Initial f))

data Final f = Final{ unFinal:: (f (Final f)) }

-----------------------------------------------------
-- The exam starts here
-----------------------------------------------------
              
-------------------------------
-- Question #1   Type Classes

-- A type, t,  belongs to the Pretty class if there
-- exists a function, pretty, of type: t -> Doc.

-- A. Write a class definition for Pretty (10 points)


-- B. Write an overloaded function showP:: Pretty t => t -> String (5 points)


-- C. Give a Pretty instance for the Exp type that uses indentation
--    to make large Exp's readable. (15 points)


-- C. Give a Show instance for Exp. (5 points)


    
-------------------------------
-- Question #3  Monads

-- A. Use the ReaderT monad transformer to define an Env
-- monad and its "run" operation. The Env should carry
-- an implict value of the function type (String -> Integer).

type Env a = Undefined  -- (10 points) 


run :: Env x -> (String -> Integer) -> x   --(5 points)
run = undefined


-- B. Write a simple val function for Exp, that encodes
-- the values of "let" bound variables in the Env monad.
-- Hint: you will need to use morphisms from 
-- Control.Monad.Reader module.  (15 points)

eval :: Exp -> Env Integer
eval = undefined

-- C. Define a simple Exp which uses, at a minimum, each of
-- the constructors of Exp at least once.  (5 points)

exp1 :: Exp
exp1 = undefined

-- D. Test your eval function by running it on exp1 (10 points)

test = undefined

-------------------------------
-- Question #4 Parsers

-- A. The LanguageDef datatype and the makeTokenParser (in Parsec) create simple parsers
-- for parsing formal languges (like identifiers, operators, literal constants).
-- This mechanism is complicated because it attempts to integrate language
-- specific comments (like  {- ... -}) into the white space routines. If we
-- do not wory about comments, we can do things much simpler. We can build
-- everything we need using just the Parsec operators
-- many,many1,satisfy,string,(<|>),chainl1,(<?>),unexpected. Note these are 
-- the only parsers imported from the Parsec package above. Note also
-- that some simple functions for RUNNING parsers are already defined above.
-- In this question you'll redefine some simple parsers that do exactly
-- as their old counterparts, but don't have any notion of comments
-- wrapped up in what it means to be white space.

-- A. write a parser for contiguous white space (space, tabs, newlines etc.) (10 points)
whiteSpace :: MParser String
whiteSpace = undefined

-- B. Write a higher order parser that skips white space after
-- running an arbitrary parser. (5 points)
lexeme:: MParser b -> MParser b
lexeme p = undefined

-- C. All the parsers in part C should handle trailing white space. (5 points each)
-- C.1  A parser for identifiers that start with a lower case letter
--      and continues with possibly many lower case, uppercase, or digit.

ident :: MParser String
ident = undefined

-- C.2 An Integer literal in base 10.  Hint. use the "read" function
number:: MParser Integer
number = undefined

-- C.3 A symbol matching (exactly) the given input string.
symbol:: String -> MParser String           
symbol s = undefined  

-- C.4 Write a higher order parser that surrounds another parser
-- with parentheses. Handles trailing white space on the open
-- and close parentheses.

parens:: MParser a -> MParser a
parens p = undefined

-- D. Write a parser for Exp. Your parser should parse whatever
-- format is printed by the Pretty class, given in Question #1 above.
-- It also should handle that Mult, and Add have the same
-- precedence, and are left associative. It should handle 
-- parentheses to specify other orderings. NOTE that your
-- parser expression is NOT expected use the layout rule. 
-- You may want to write and use "helper" functions. (20 points)
 
expP:: MParser Exp
expP = undefined 
                    

-- E. Write some code here that tests that (parse2 expP (showP x) == x)
-- Use a value of type Exp that uses, at a minimum, each of
-- the constructors of Exp at least once. (5 points)

test2 = undefined

---------------------------------------------
-- Question #7 Algebras and Co-Algebras

-- A. Define a functor E that has the same shape as Exp.
-- Use the same names for constructors of E as Exp uses, except
-- add the letter "E" to the end of the name. I.e. VarE instead of Var
-- Be sure E is an instance of class Functor. (10 points)

data E x = Undefined
         
-- B. Define a type synonym Exp2 which is isomorphic to Exp 
-- which is the initial algebra of E. (5 points)

type Exp2 = Undefined

-- B. Define a type synonym Exp3 which is the Final algebra of E. (5 points)

type Exp3 = Undefined

-- C. Define 2 functions (f,g) with the following type (E Exp2 -> E Exp2)
-- each of which implements the following  identities over arithmetic
-- (represented by Exp2 data) (20 points total, 10 each)
-- 
-- f (0 + x) = x
-- f (x + 0) = x
-- f (x + y) = (x + y)
-- 
-- g (0 * x) = 0
-- g (x * 0) = 0
-- g (1 * x) = x
-- g (x * 1) = x
-- g (x * y) = (x * y)

f:: E Exp2 -> E Exp2
f = undefined

g:: E Exp2 -> E Exp2 
g = undefined

-- D. Write a function that applies these identities everywhere
-- in bottom up traversal of an Exp2. Hint use "fmap" (20 points)

bottomUp :: Exp2 -> Exp2
bottomUp = undefined
    
-------------------------------
-- Question #6 Arrows

newtype Trans a b = Trans (a -> Maybe b)

-- The type Trans is meant to indicate a transformation
-- from type a to type b. Implict in this type is the fact
-- that a transformation might fail. That is why the range
-- is a Maybe type. Of particular interest is representing
-- rewriting rules as transformations of type (Trans x x).
-- Here the source and target are the same type. For example 
-- consider the function "fz" below, which implements
-- the transformations   (x + 0) --> x,   and    (0+x) --> x.

fz :: Exp -> Maybe Exp
fz (Add x (Int 0)) = Just x
fz (Add (Int 0) x) = Just x
fz _ = Nothing

trans1 :: Trans Exp Exp
trans1 = Trans fz

-- A. Implementent the transformation, trans2, for the following (10 points)
-- (x * 0) --> 0
-- (0 * x) --> 0
-- (1 * x) --> x
-- (x * 1) --> x.

trans2 :: Trans Exp Exp
trans2 = undefined

-- B. Implement the transformation combinator, try,
-- which applies each transformation in a list. It
-- stops, and returns the value of, the first 
-- transformation that succeeds. (20 points)

try:: [Trans a b] -> Trans a b
try xs = undefined
             
-- We see that Transformations form a category.
instance Category Trans  where
  id = (Trans Just)
  (Trans f) . (Trans g) = Trans h
    where h x = do { y <- g x; f y}
    
-- C. Show that Transformation is also an instance of the Arrow Class.  (15 points)           

instance Arrow Trans where 
  arr f = undefined
  first rtans = undefined
  

             



                                        

