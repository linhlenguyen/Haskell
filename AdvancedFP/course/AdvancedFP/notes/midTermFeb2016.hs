{-# Language DeriveFunctor, FlexibleInstances, FlexibleContexts,
    GADTs, KindSignatures, MultiParamTypeClasses #-}

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
-- that keeps it from loading.

-- Each question is assigned a number of points proportional
-- to the difficulty of the question. There is a total of 275
-- points. No one is expected to come close to doing the whole
-- exam.

-----------------------------------------------------------
-- All the imports you should need
-----------------------------------------------------------

-- import the Hughes library qualified 
import qualified Text.PrettyPrint.HughesPJ as PP
-- import a few widely used operations without qualification
import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($$),($+$),render) 

-- These are for defining parsers
import Text.Parsec(ParsecT(..),runParserT
                  ,satisfy,string,char,many,(<|>)
                  ,(<?>),unexpected)  
import Text.Parsec.Prim(getState,getInput)
-- This is for possible Monads underlying the Parsing Monad
import Text.Parsec.Combinator

-- This is for operations on simple Data
import Data.Char(digitToInt,isUpper,isLower,isAlpha,isDigit,isSpace)

-- Monad transformers
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer hiding ((<>))


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

cata :: Functor f => (Algebra f b) -> Initial f -> b
cata (Algebra phi) (Init x) = phi(fmap (cata (Algebra phi)) x)

--------------------------------------------
-- Initial and Final Algebras

data Initial f = Init (f (Initial f))

data Final f = Final{ unFinal:: (f (Final f)) }

-----------------------------------------------------
-- The exam starts here
-----------------------------------------------------
              
-------------------------------------------------------------
-- Parsing
-------------------------------------------------------------

-- (1) Write a parser for a sequence of white space characters
-- hint: use "sat" defined above. 5 points.

whiteSpace:: MParser String
whiteSpace = undefined

-- (2) write a parser that consumes trailing white space after 
-- parsing something else specified by an input parser. 5 points

lexeme:: MParser a -> MParser a
lexeme p = undefined

-- (3) write a symbol parser that parses only the string given as input
-- then consumes trailing whitespace. 5 points

symbol:: String -> MParser String
symbol s = undefined

-- Consider the regular expressions over alphabetic characters.
-- represented by the datatype below. A regular expression is
-- meant to denote a set of strings over the characters
-- ['a' .. 'z']  and ['A' .. 'Z']. Tis can be enforced by
-- applying One to only alphabetic characters. This is enforced
-- by convention.

data RegExp 
  = Epsilon                        -- the empty string {""}
  | Empty                          -- the empty set    {}
  | One Char                       -- a singleton set {a}
  | Union RegExp RegExp            -- union of two RegExp
  | Cat RegExp RegExp              -- concatenation
  | Star RegExp                    -- kleene closure
     deriving (Ord,Eq)


-- A regular expression string to be parsed should contain only the alpha
-- characters "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
-- meta characters "()+*#$"  and white space.
-- "(" and ")" are used for grouping, "+" for union, "*" is used
-- as postfix kleene closure, "#" is epsilon (the empty string),
-- and "$" is the emptyset. Concatenation is indicated by juxtaposition.
-- "*" binds with highest precedence, then concatenation, then "+"
-- with lowest precedence.  Note that concatenation and union 
-- are associative.

-- some examples

s1 = "a b+c* (#+ w)"
r1 = Union (Cat (One 'a') (One 'b'))
           (Cat (Star (One 'c'))
                (Union Epsilon (One 'w')))
                
s2 = "$ a"
r2 = Cat Empty (One 'a')

-- (4) write a parser for regular expressions over 
-- alphabetic characters. We will use layered parsers
-- structured by precedence rules. simpleRE (one token)
-- parsers, starRE (parsers for the operator with highest precedence)
-- catRE to parse concatenation, indicated by juxtaposition (the next
-- highest parsers) and finally, reqexp (union parsers, the operator
-- with lowest precedence). 50 points

simpleRE:: MParser RegExp
simpleRE = undefined
        
                                                  
starRE:: MParser RegExp
starRE = undefined

catRE:: MParser RegExp
catRE = undefined
    
regexp:: MParser RegExp  -- This is union parsers
regexp = undefined

-------------------------------------------------------------
-- Pretty Printing
-------------------------------------------------------------

-- (5) Write a pretty printer for RegExp. Make some effort to
--    (a) Make pretty printing followed by parsing the identity
--    (b) Not to print extra parentheses that are unneeded 
--        because of the precedence and associativity rules.
-- 30 points


 

ppRE:: RegExp -> Doc
ppRE x = undefined

-- (6) Use ppRE to make (RegExp Char) an instance of
-- the Show class.
        
-- write a class declaration for Show here
  

---------------------------------------------------------
-- Writing functions by pattern matching
---------------------------------------------------------
  
-- (7) Write a function that given a regular expression
-- returns True if the input regular expression accepts 
-- the empty string (Epsilon) and False otherwise. 10 points

emptyString:: RegExp -> Bool
emptyString x = undefined


-- (8) Write the function "deriv" it is given two inputs
--   (a) a regular expression (X)
--   (b) a character (c)
-- It returns another regular expression (Y) such that:
-- (X accepts the string (c:cs)) if and only if (Y accepts cs).
-- Think of the answer Y as the regular expression that
-- accepts the tail of the string cs, if X can accept
-- the initial character 'c'.  If none of the set of strings
-- accepted by X can start with 'c', then return Empty (which
-- accepts no strings).
-- Hint, the function emptyString (above) plays a role in
-- the concatenation case. 40 points

deriv:: RegExp -> Char -> RegExp 
deriv x c = undefined



-- (9) Write a function "recog" which such that (recog r s) 
-- returns True if "r" accepts "s", and False otherwise.
-- hint: Use the functions "emptyString" and  "deriv" above. 10 points

recog:: RegExp -> String -> Bool
recog r s = undefined

---------------------------------------------------
-- GADT's  and generic programming
---------------------------------------------------

-- The Rep type can be used to write generic
-- programs. One program defined over all types in 
-- represented in a universe. Example types include
-- Int, Bool, (Int,Char), [Char], [(Bool,[[Int]])]

 
data Rep:: * -> * where
 Int:: Rep Int
 Char:: Rep Char
 Bool :: Rep Bool
 Pair:: Rep a -> Rep b -> Rep (a,b)
 List :: Rep a -> Rep [a]

-- Recall the ordering type
-- data Ordering = LT | EQ | GT
--
-- used to compare two elements of the same type
-- and the Ord class
--
-- class Ord x where
--   compare:: x -> x -> Ordering

-- (10) Write the generic equality and comparison operators. 20 points total

eql :: Rep a -> a -> a -> Bool  -- 8 points
eql rep x y = undefined
 

cmpr :: Rep a -> a -> a -> Ordering   -- 12 points
cmpr rep x y = undefined
 
------------------------------------------------------
-- Haskell Classes
------------------------------------------------------

-- Consider the Haskell Class Generic

class Generic t where
  repOf:: Rep t
  
-- with one method "repOf", a type 't' is generic
-- if there exists a constant of type  (Rep t)

-- (11)  Fill in the following class instances. 20 points
-- uncomment them and fill them in

{-
instance Generic Int where
   
  
instance Generic Char where
  
  
instance Generic Bool where
  
  
instance (Generic a,Generic b) => Generic (a,b) where
  
  
instance Generic x => Generic [x] where
-}   
  
-- (12) write the following functions (and test them). 20 points

equality:: Generic x => x -> x -> Bool
equality x y = undefined

comparison:: Generic x => x -> x -> Ordering
comparison x y = undefined

------------------------------------------------------
-- Monads and classes
------------------------------------------------------

-- Consider the class Lang below

class Monad m => Lang e m v where
  eval:: e -> m v

-- the class Lang relates three types
--   (a) a Monad m
--   (b) a datatype that represents a language term
--   (c) a type that represents a value, or meaning of the term

-- The idea is that a language term is given a meaning
-- as a monadic computation. Example is the RegExp terms.
-- The meaning of a regular expression is a function from
-- String -> Bool, that tests if the string is a member of
-- the set of strings denoted by the regular expression.
-- Here the monad is the identity monad since computing
-- the (String -> Bool) function has no side effects.
  
instance Lang RegExp Identity (String -> Bool) where
  eval e = Identity(\ s -> recog e s)
  
-- (12) Create an instance of Lang for 3 types you invent. 50 points total

-- 
-- (a) Invent an algebraic datatype of terms for some language
-- whose meaning will be something non trivial (i.e. has some effect). 10 points

data Term = Term Undefined
  
-- (b) Choose a non trivial monad (I.e. something not the Identity)
-- and define it using monad transformers. Replace Identity with your choice. 10 points

type M = Identity

-- (c) Choose a type of values meant to be the meaning of 
-- the terms under the effects. 10 points

type Value = Undefined

-- (d) Create a valid Lang instance. 20 points

instance Lang Term M Value where
  eval = undefined
  
 
  
------------------------------------------------------
-- Algebras 
------------------------------------------------------

-- Consider the algebraic data type of Trees.
-- Trees are useful for binary search if the shape
-- of the tree respects the binary serach invariant.
-- Ie. for every fork tree (Fork left n right), the 
-- values stored in left are less than n, and the 
-- values stored in right are greater than n.

data Tree = Fork Tree Int Tree | Tip

-- (13) Define a functor appropriate for incoding Tree
-- as an Initial Algebra.  Be sure and make T an instance
-- of the Functor class. 5 points

data T x = T Undefined
   

-- (14) Define a type synonym using Initial algebras
-- that is isomorhpic to Tree. 5 points

type Tree2 = Undefined

-- (15) Demonstrate that Tree and Tree2 are indeed
-- isomorphic by writing two functions that are each
-- other's inverses. 10 points

into :: Tree -> Tree2
into x = undefined  

out:: Tree2 -> Tree
out x = undefined  
 
-- (16) Write a function that flattens a Tree2 into
-- a list of Int. Define it using "cata" 15 points.

flat :: Tree2 -> [Int]
flat x = undefined
        
-- (17) Write a function that tests if a Tree2
-- meets the binary search invariant. Define it using "cata"
-- This function requires returning a pair.
-- The defining equations are:  
-- if (invariant t) evaluates to (b,t2) then
--   (1) b is True if and only if t meets the invariant, and 
--   (2) t==t2
-- 20 points

invariant :: Tree2 -> (Bool,Tree2)
invariant x = undefined