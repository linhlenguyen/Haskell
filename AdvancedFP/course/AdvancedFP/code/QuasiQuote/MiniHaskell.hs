{-# Language TemplateHaskell,FlexibleContexts #-}

module MiniHaskell where

-- Quasi Quoting means inserting small Haskell fragments in
-- the escapes of quasiquoted Object languages (i.e. Datalog formulas).
-- For example [form| f(x,y), g($(call 3),z) |]. Where (call 3)
-- is a haskell expression that evaluates to an object language expression.
-- This requires the quasiquoters to
-- parse these Haskell fragments as Template Haskell (TH.Exp)
-- The goal is to eventually parse all Haskell expressions 
-- that have no decal bindings or layout (let, where, do, case),
-- as bindings and layout cause cause problems.  We feel that 
-- it is unlikely that an escape will have these kind of expressions
-- so this fragment of Haskell should be sufficient.

import Text.Parsec hiding (State,Column)
import Text.Parsec.Expr(Operator(..),Assoc(..),buildExpressionParser)
import Text.Parsec.Prim(getInput)
import Text.Parsec.Token
import Text.Parsec.Language(haskellDef)
import Control.Monad.State
import Data.Char(digitToInt,isUpper)
import Data.Functor.Identity(Identity(..))
import qualified Control.Exception
import Data.Either (lefts)
import Data.Char(isUpper)

import qualified Language.Haskell.TH as T

-------------------------------------------------------

-- create a haskell token parser
haskell = makeTokenParser haskellDef

------------------------------------------------------
-- Infix operators
-- We will define just a few of the possible 
-- infix operators of Haskell. Create a datastructure 
-- parameterized by two functions. This has two uses
-- Use (1) obtain a list of all operators
--     (2) Create a structure for input to buildExpressionParser

opList prefix infiX =
    [ [ prefix "~"],
      [ infiX "!!" AssocLeft, infiX "." AssocLeft]
    , [ infiX "^"  AssocRight]
    , [ infiX "*"  AssocLeft, infiX "/"  AssocLeft]
    , [ infiX "+"  AssocLeft, infiX "-"  AssocLeft]
    , [ infiX ":" AssocRight]
    , [ infiX "++" AssocRight]
    , [ infiX "==" AssocNone, infiX "/=" AssocNone, infiX "<"  AssocNone
      , infiX "<=" AssocNone, infiX ">"  AssocNone, infiX ">=" AssocNone ]
    , [ infiX "<=>" AssocNone ]
    , [ infiX "&&" AssocRight ]
    , [ infiX "||" AssocRight, infiX "=>" AssocRight ]
    , [ infiX "$" AssocRight ]
    , [ infiX "?" AssocRight]
   ]


-- Use (1) a list of all infix operators (prefix are filtered out)
miniHaskellOps = filter (/="") (concat (opList prefix op))
  where prefix x = ""
        op x y = x  
        
-- Parse a valid infix operator.
-- This is used when parsing Sections, like (+) and (4+)
oper:: ParsecT String u Identity T.Exp
oper = try $
  do { c <- choice (map (lexeme haskell . string) miniHaskellOps)
     ; case hOp c of
         Just x -> return x
         Nothing -> error ("not an operator") }


                
-- Use (2) Create a structure for input to buildExpressionParser

operatorTable :: [[Operator String u Identity T.Exp]]
operatorTable = opList prefix infiX    

prefix name = Prefix(do{ try (reservedOp haskell name); exp2exp name })   
  where exp2exp "~" = return neg 
        exp2exp other = unexpected ("Unknown prefix operator: "++other)
        neg (T.LitE (T.IntegerL x)) = T.LitE (T.IntegerL (negate x))
        neg (T.LitE (T.RationalL x)) = T.LitE (T.RationalL (negate x))
        neg x = T.AppE (T.VarE 'negate) x

infiX nam assoc = 
   Infix (do{ pos <- getPosition; try (reservedOp haskell nam); exp2exp2exp pos nam}) assoc
  where exp2exp2exp loc "$" = return T.AppE
        exp2exp2exp loc nam = return (binOp nam (hOp nam))
        binOp nam Nothing x y = error ("unknown infix operator " ++nam) 
        binOp nam (Just oper) x y = T.InfixE (Just x) oper (Just y)


----------------------------------------------------
-- Parsing names or identifiers

idString = identifier haskell   
sym = symbol haskell

nameP =
  do { str <- idString ; return(stringToExp str) }
    
stringToExp str =
  if isUpper (head str)
     then (T.ConE (T.mkName str))
     else (T.VarE (T.mkName str))
     
stringToPat str =
  if isUpper (head str)
     then (T.ConP (T.mkName str)[] )
     else (T.VarP (T.mkName str))     

-- Assign a real Haskell T.Name to every operator.  
hOp "!!" = Just (T.VarE '(!!))
hOp "."  = Just (T.VarE '(.))
hOp "^"  = Just (T.VarE '(^))
hOp "*"  = Just (T.VarE '(*))
hOp "/"  = Just (T.VarE '(/))
hOp "+"  = Just (T.VarE '(+))
hOp "-"  = Just (T.VarE '(-))
hOp ":"  = Just (T.ConE '(:))
hOp "++" = Just (T.VarE '(++))
hOp "==" = Just (T.VarE '(==))
hOp "/=" = Just (T.VarE '(/=))
hOp "<"  = Just (T.VarE '(<))
hOp "<=" = Just (T.VarE '(<=))
hOp ">"  = Just (T.VarE '(>))
hOp ">=" = Just (T.VarE '(>=))
hOp "&&" = Just (T.VarE '(&&))
hOp "||" = Just (T.VarE '(||))
hOp "$"  = Just (T.VarE '($))
hOp x    = Nothing
         
----------------------------------------------------
-- Parsing Haskell literals
               
chrLit  = do{ c <- charLiteral haskell; return (T.CharL c) }
doubleLit = do { n <- (signed (float haskell)); return(T.RationalL (toRational n))}
intLit = do{ c <- lexeme haskell (decimal haskell); return (T.IntegerL c) }
strLit = do{ s <- stringLiteral haskell; return(T.StringL s)}

literal = lexeme haskell
   (try doubleLit)  <|>  -- float before natP or 123.45 leaves the .45
   (try intLit)     <|>  -- try or a "-" always expects digit, and won't fail, "->"
   (try strLit)     <|>
   chrLit           <?> "literal"

-- Literals as Expressions
literalExp = (literal >>= return . T.LitE)

-- Literals as Patterns
literalPat = fmap T.LitP literal

signed p = do { f <- sign; x <- p; return(f x) }

sign:: (Stream s m Char,Num t) => ParsecT s u m (t -> t)
sign = (char '~' >> return negate) <|> (return id)  

----------------------------------------------------   
-- Simple expressions have no applications

--             x       ( exp )     [3,4]    5, "abc", 'z'
simpleExpP = nameP <|> parenP <|> listP <|> literalExp


-- Parenthesized things include tuples, unit, and sections

parenP = sectionP <|> tupleP   

                                    
-- (+), (* 5) (3 +) (,) etc
sectionP :: ParsecT String u Identity T.Exp
sectionP = try operator <|> try left <|> try right <|> try pairOper
  where operator = (do { l <- getPosition
                       ; symbol haskell "("; z <- oper; symbol haskell ")"
                       ; return(T.InfixE Nothing z Nothing)})
                                                               
        left =(do { l <- getPosition
                  ; symbol haskell "("; z <- oper; y <- expP; symbol haskell ")"
                  ; return (T.InfixE Nothing z (Just y))})
        right = (do { l <- getPosition
                    ; symbol haskell "("; y <- simpleExpP; z <- oper; symbol haskell ")"
                    ; return (T.InfixE (Just y) z Nothing)})

        pairOper = (do { l <- getPosition
                       ; parens haskell (comma haskell)
                       ; return (T.ConE '(,))})

-- Normal parenthesized things like tuples and unit                       
tupleP = 
  do { es <- parens haskell (sepBy expP (comma haskell))
     ; case es of
         [] -> return(T.ConE '())
         [e] -> return e
         es -> return(T.TupE es)
     } 
     
-- Things surrounded by brackets, They all start with 1 or more expP
-- 1) lists like [2,3,4]
-- 2) generators like  [3..], [3 .. 10], [3,5 ..], [3,5 .. 10]   
-- 3) Comprehensions like [ (x,y) | x <- a, y <- b]

listP = brackets haskell list
  where list = do { xs <- sepBy expP (comma haskell); tail xs}
        tail []    = close []
        tail [x]   = count [x] <|> comp x <|> close [x]
        tail [x,y] = count [x,y] <|> close [x,y] 
        tail xs    = close xs
        close xs = return(T.ListE xs)
        count xs = do { sym ".." 
                      ; suffix <- fmap Just expP <|> (return Nothing)
                      ; case (xs,suffix) of
                         ([x],Nothing)   -> return(T.ArithSeqE (T.FromR x))
                         ([x],Just z)    -> return(T.ArithSeqE (T.FromToR x z))        
                         ([x,y],Nothing) -> return(T.ArithSeqE (T.FromThenR x y))
                         ([x,y],Just z)  -> return(T.ArithSeqE (T.FromThenToR x y z)) }
        comp x = do { sym "|"
                    ; xs <- sepBy bind (sym ",")
                    ; return(T.CompE (xs ++ [T.NoBindS x]))}
        bind = try gen <|> fmap T.NoBindS expP
        gen = do { p <- patP
                 ; sym "<-"
                 ; e <- expP
                 ; return(T.BindS p e)}
                 

applyExpression =
    do { exprs <- many1 simpleExpP
       ; let apply [] =  unexpected "empty Exp list"
             apply [x] = return x
             apply (x:y:xs) = apply (T.AppE x y : xs)
      ; apply exprs } 
  
infixExpression = buildExpressionParser operatorTable applyExpression
  
--------------------------------------------------------------

-------------------------------------------------------
-- Expressions (decl bindings) are either infix 
-- (which includes applications) or lambdas, or ifs

ifExpP =
  do { reserved haskell "if"
     ; test <- expP
     ; reserved haskell "then"
     ; x <- expP
     ; reserved haskell "else"
     ; y <- expP
     ; return(T.CondE test x y)}

-- \ x -> f x

lambdaExpression =
    do{ reservedOp haskell "\\"
      ; ps <- many1 patP
      ; sym "->"
      ; body <- expP
      ; return(T.LamE ps body) }


expP = ifExpP
    <|> lambdaExpression
    <|> infixExpression     --names last
    <?> "expression"
    

-------------------------------------------------
-- Patterns
    
patP = try constrPat <|> infixPat  <|> annPat 

-- things like "(a,b) : xs"  or "x @ (a,b)" 
-- or just patterns without and following operator
infixPat = do { p <- simplePat; oper p <|> return p}
  where oper p = do { x <- (symbol haskell "@") <|> (operator haskell)
                    ; case (p,x) of
                       (p,':' : xs) -> do { q <- patP; return(T.InfixP p (T.mkName x) q)}
                       (T.VarP nm,"@") -> do {q <- patP; return(T.AsP nm q)}
                       (p,y:ys) -> unexpected("infix pattern where operator: "++x++" does not start with ':'")
                    }

-- things like "~(x:xs)"  or "!(C a b)"
annPat = do { tag <- patAnnotation; p <- patP; return(tag p)}
patAnnotation = (symbol haskell "~" >> return T.TildeP) <|>
                (symbol haskell "!" >> return T.BangP)     


-- things like 3, "abc", 'z', x, _, (p,q), [p,q]
simplePat = literalPat <|> var <|> wild <|> tuplePat <|> listPat
  where var = do { s <- identifier haskell; return(stringToPat s)}
        wild = (symbol haskell "_" >> return(T.WildP))
                    
tuplePat = fmap tup (parens haskell (sepBy patP (comma haskell)))
   where tup [x] = x
         tup xs = T.TupP xs
         
listPat = fmap T.ListP (brackets haskell (sepBy patP (comma haskell)))
         
-- things like "C" or "C x y z"                                                     
constrPat = 
  do { c <- identifier haskell
     ; if (isUpper (head c))
          then (do { ps <- many1 patP; return (T.ConP (T.mkName c) ps)})
          else (unexpected ("non constructor in pattern: "++c))}
          
---------------------------------------


              
                    