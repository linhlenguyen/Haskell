{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoMonomorphismRestriction #-}

module Parser where

import Literal hiding (try)
import Syntax
import Dimension
import Value(initEnumerations,to)
import FiniteSet(SetI(..))
import Auxfuns (plistf)
-- import Types(Typ(..),Kind(..),toTyp,tupleT,arrT,applyT)
import qualified System.IO
import qualified System.IO.Error
import Data.List(groupBy)
-- import Data.Functor.Identity(Identity)

import Data.Char(digitToInt,isUpper)

-- These are for defining parsers
import Text.Parsec  hiding (State,try)
import Text.Parsec.Expr(Operator(..),Assoc(..),buildExpressionParser)
import Text.Parsec.Prim(ParsecT,Parsec,try,runParserT)
import Control.Monad.State 
import LayoutToken2 -- Replaces Text.Parsec.Token
                   -- and adds layout rule parsing capability



--------------------------------------------------------------------
-- We are specializing the generic Parsec Parser Transformer here

type InternalState = (String -> Maybe String)
initState = (initEnumerations)
type MParser a = ParsecT
                    String                -- The input is a sequence of Char
                    [Column]              -- The internal state for Layout tabs
                    (State InternalState) -- The other internal state: type and kind mappings
                    a                     -- the type of the object being parsed

-----------------------------------------------
-- running parsers

parse1 :: MParser a -> String -> (Either ParseError a, InternalState)
parse1 x s = runState (runParserT (whiteSp >> x) [] "Test string" s) initState

parse2 x s =
  case parse1 x s of
   (Right ans,st) -> ans
   (Left message,st) -> error (show message)

parseString x s =
  case parse1 x s of
   (Right ans,st) -> return ans
   (Left message,st) -> fail (show message)   

augmented p = do { ans <- (whiteSp >> p); s <- get; return(ans,s)}

parseFile parser file =
  do {  possible <- System.IO.Error.tryIOError (readFile file)
     ; case possible of
         Right contents -> 
            case runState (runParserT (augmented parser) [] file contents) initState of
            -- runParser (augmented parser) initState file contents of
              (Right ans,st) -> return ans
              (Left message,st) -> error(show message)
         Left err -> error(show err) }

-- A special parser-transformer for seeing what input is left

observeSuffix x = 
  (do { a <- (whiteSp >> x)
      ; (col,tabs,left) <- getInfo
      ; return(a,col,tabs,take 20 left)})
      
parseAll pstate mess x s = 
  case runState (runParserT (observeSuffix x) [] mess s) initState of
    (Right(ans,_,_,[]),st) -> return ans
    (Right(ans,_,_,suffix),st) -> error ("\nUnexpected trailing stuff after parse: '"++
                                   suffix++"'\nWhile reading from: '"++mess++"'")
    (Left message,st) -> fail (show message)


ps x s = parse2 (observeSuffix x) s
--------------------------------------------         
teststr = "f x = (let anc(x,y) -> d(x), d(y).\n             anc(x,y) <- $f(x,y); $f(x,z),anc(x,y). in anc)\ng x = 1"


-- use the internal state
getEnumType :: String -> MParser String
getEnumType s = 
            do { f <- get
               ; case (f s) of
                  Nothing -> unexpected (s++" is not a known enumerated constant.")
                  Just d -> return d }

-- This builds a style similar to Haskell but where the key words
-- are specialized to the LB ones (see reservedNames below).
        
lbStyle :: GenLanguageDef String [Column] (State InternalState)
lbStyle = LanguageDef
                { commentStart   = "{-"
                , commentEnd     = "-}"
                , commentLine    = "--"
                , nestedComments = True
--                , identStart     = letter
                , identStart     = lower
                , identLetter    = alphaNum <|> oneOf "_'"
                , opStart              = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , caseSensitive  = True
                , reservedOpNames = []
                 -- ["!","?","\\",":",".", "<", "=", "+", "-", "^", "()", "_", "@"]
                , reservedNames  = 
                  ["if","then","else","case","of","let","in","by"
                  -- ,"one","some","none","full"
                  ,"fundep"
                  ,"find","dim","set", "exists", "where"
                  ,"narrow", "forall"]
                }

-- Now create a token parser and a layout function.
-- The token parser is used to create Style dependent parsers

(funlog,LayFun layout) = makeTokenParser lbStyle "{" ";" "}"
                
------------------ Style and Style dependent tokens ----------------

lexemE p    = lexeme funlog p
arrow       = lexemE(string "->")
larrow      = lexemE(string "<-")
dot         = lexemE(char '.')
under       = char '_'
parenS p    = between (symboL "(") (symboL ")") p
braceS p    = between (symboL "{") (symboL "}") p
bracketS    = brackets funlog
symboL      = symbol funlog
natural     = lexemE(number 10 digit)
whiteSp     = whiteSpace funlog
ident       = identifier funlog
sym         = symbol funlog
keyword     = reserved funlog
commA       = comma funlog
resOp s     = reservedOp funlog s 
oper        = operator funlog 
exactly s err  =
   do { t <- ident
      ; if s==t then return s 
                else unexpected("\nExpecting the name: "++show s++"\nbut found the name: "++show t++err)}

number :: Integer -> MParser Char -> MParser Integer
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }


-------------------- Names and identifiers --------------------------

nameP = do { pos <- getPosition; x <- ident; return(Nm(x,pos))}

conName :: MParser String
conName = lexemE (try construct)
  where construct = do{ c <- upper
                      ; cs <- many (identLetter lbStyle)
                      ; return(c:cs)}
                   <?> "Constructor name"
                    
                    
atomName = lexeme funlog (try construct) <|> lexeme funlog (try equal)
  where construct = do{ cs <- many (identLetter lbStyle)
                      ; char '('
                      ; return cs }
                    <?> "Atom name" 
        equal = do { string "eq#"
                   ; cs <- many (identLetter lbStyle)
                   ; char '('
                   ; return ('#':cs) } <?> "Equality predicate"


patvariable :: MParser Pat
patvariable = 
  do { x <- nameP
     ; let result = (PVar x)
     ; let (patname@(init:_)) = name x
     ; if isUpper init
          then fail ("pattern bindings must be lowercase, but this is not: " ++ patname)
          else return result}

expvariable  :: MParser Expr               
expvariable = do { s <- nameP; return(EVar s)}

expconstructor  :: MParser Expr
expconstructor = do { pos <- getPosition; s <- conName; return(EVar (Nm(s,pos)))}

---------------------------------------------------------
-- Parsers for Literals

signed p = do { f <- sign; x <- p; return(f x) }
  where sign = (char '~' >> return negate) <|>
               (return id)   
               
chrLit  = do{ c <- charLiteral funlog; return (LChar c) }
doubleLit = do { n <- (signed (float funlog)); return(LDouble n)}
intLit = do{ c <- (signed Parser.natural); return (LInt (fromInteger c)) }
strLit = do{ s <- stringLiteral funlog; return(LString s)}

literal :: MParser Literal
literal = lexeme funlog
   (try doubleLit)  <|>  -- float before natP or 123.45 leaves the .45
   (try intLit)     <|>  -- try or a "-" always expects digit, and won't fail, "->"
   (try strLit)     <|>
   chrLit           <?> "literal"


--------------------- Parsing Patterns ----------------------------

simplePattern :: MParser Pat
simplePattern =
        tuplePattern
    <|> domainPattern        
    <|> (do { pos <- getPosition; x <- literal; return(PLit pos x)}) 
    <|> (do { pos <- getPosition; sym "_"; return (PWild pos)})
    <|> (do { pos <- getPosition; nm <- conName; return(PCon (Nm(nm,pos)) []) })
    <|> (do { e <- escape; return(PEsc e)}) 
    <|> listPattern
    <|> patvariable
    <?> "simple pattern"

-- () (x)  (x,y,z)
tuplePattern = 
  do { xs <- parenS(sepBy pattern (commA))
     ; return(patTuple xs) 
     }

domainPattern = 
  do { sym "#"
     ; xs <- parenS(sepBy pattern (commA))
     ; return(PTuple DIM xs) 
     }     

infixPattern =
  do { p1 <- try conApp <|> simplePattern
                    --  E.g. "(L x : xs)" should parse as ((L x) : xs) rather than (L(x:xs))
     ; x <- sym ":"
     ; pos <- getPosition; 
     ; p2 <- pattern
     ; return (PCon (Nm(x,pos)) [p1,p2])
     }

conApp =
   (do { pos <- getPosition; 
       ; cname <- conName
       ; ps <- many simplePattern
       ; case ps of
          [] -> do { f <- get
                   ; case (f cname) of
                      Nothing -> unexpected (cname++" is not a known enumerated constant.")
                      Just ty -> return (PLit pos (LCon ty cname)) }
          _  -> return (PCon (Nm(cname,pos)) ps)})
     
-- [2,3,4]
listPattern = 
  do { xs <- bracketS (sepBy pattern (commA))
     ; return(pConsUp patNil xs)
     }

pattern :: MParser Pat
pattern = try infixPattern
  <|> conApp
  <|> simplePattern
  <?> "pattern"


------------------------------------------------------
-- Defining Infix and Prefix operators
-- See  "opList"  in Syntax.hs

-- operatorTable :: [[Operator Char [Column] Expr]]
operatorTable = opList prefix infiX    

-- prefix :: String -> Operator Char [Column] Expr
prefix name = Prefix(do{ try (resOp name); exp2exp name })   
  where -- exp2exp:: String -> Expr -> Expr
        exp2exp "~" = return neg
        exp2exp other = fail ("Unknown prefix operator: "++other)
        neg (ELit p (LInt x)) = ELit p (LInt (negate x))
        neg (ELit p (LDouble x)) = ELit p (LDouble (negate x))
        neg x = (EApp (EVar (Nm("negate",prelude))) [x])

infiX nam assoc = 
   Infix (do{ pos <- getPosition; try (resOp nam); exp2exp2exp pos nam}) assoc
  where exp2exp2exp loc ":" = return consExp
        exp2exp2exp loc "$" = return (\ x y -> EApp x [y])
        -- exp2exp2exp loc "?" = return (\ x y -> EChoose Choose x y)
        exp2exp2exp loc nam = return (binop (Nm(nam,loc)))

infixExpression:: MParser Expr
infixExpression = buildExpressionParser operatorTable applyExpression

-- f x (3+4) 9
applyExpression:: MParser Expr
applyExpression =
    do{ exprs <- many1 simpleExpression
      ; return (applyE exprs)
      }    
      

---------------------- Parsing Expr ---------------------------
e1 = parse2 expr "(123,34.5,\"abc\",())"
e2 = parse2 expr "if x then (4+5) else 0"
e3 = parse2 expr "case x of\n   D x -> 5\n   C y -> y+1"
e4 = parse2 expr "\\ C x -> 4\n  D y -> g 7 y"
e5 = parse2 expr "\\ x -> \\ y -> x+ y"

expr :: MParser Expr
expr =  lambdaExpression
    <|> letExpression
    <|> setExpression
    <|> ifExpression
    -- <|> narrowExpression
    <|> infixExpression     --names last
    <?> "expression"


escExp = do { sym "$"; x <- parenS conOrForm; return(EForm x)}

-- set (i4,s3) [(1,"abc")]
setExpression :: MParser Expr
setExpression =
   do { keyword "set"
      ; ds <- simpleExpression  -- parenS(sepBy expr (commA))
      ; body <- simpleExpression
      ; return(ESet ds body)}

{-
narrowExpression =
   do { keyword "narrow"
      ; ds <- simpleExpression  
      ; body <- simpleExpression
      ; return(EChoose Narrow ds body)}
-}


-- let {p = e; f x = e} in e
letExpression :: MParser Expr
letExpression =
    do{ keyword "let"
      ; oldf <- getState
      ; decls <- layoutDecl (keyword "in")
      ; e <- expr
      ; setState oldf
      ; let letUp [] e = e
            letUp (d:ds) e = ELet d (letUp ds e)
      ; return $ (letUp ( decls) e)
      }

simpleExpression :: MParser Expr
simpleExpression = 
        parenExpression           -- (+) () (x) (x,y,z)
    <|> dimExpression             -- #(i4,names)
    <|> literalP                  -- 23.5   'x'   `d  123  #34 45n  "abc"
    <|> listExpression            -- [2,3,4]
    <|> caseExpression            -- case e of {p -> e; ...}
    <|> escExp                    -- $( g(x,y),g(x) )
    <|> expconstructor            -- a constructor name like "Node" or "Cons"
    <|> expvariable               -- names come last
    <?> "simple expression"


dimExpression = 
   do { sym "#"
      ; xs <- parenS(sepBy1 expr (commA))
      ; return(ETuple DIM xs)}

-- 23.5   123  "abc"  ()
literalP = do { loc <- getPosition; x <- literal; return(ELit loc x)}

-- (,)
pairOper :: MParser Expr
pairOper = do { l <- getPosition
              ; parenS commA
              ; return (EAbs [(PVar (Nm("_x",l))
                              ,EAbs [(PVar (Nm("_y",l))
                                     ,expTuple[EVar (Nm("_x",l)),EVar (Nm("_y",l))])])])}
                                     
-- (+), (* 5) (3 +) (,) etc
section :: MParser Expr
section = try operator <|> try left <|> try right <|> try pairOper
  where operator = (do { l <- getPosition
                       ; sym "("; z <- oper; sym ")"
                       ; return (EAbs [(PVar (Nm("_x",l))
                                ,EAbs [(PVar (Nm("_y",l))
                                       ,EApp (EVar (Nm(z,l))) [EVar (Nm("_x",l)),EVar (Nm("_y",l))])])])})
        left =(do { l <- getPosition
                  ; sym "("; z <- oper; y <- expr; sym ")"
                  ; return (EAbs [(PVar (Nm("_x",l))
                                   ,EApp (EVar (Nm(z,l))) [EVar (Nm("_x",l)),y])])})
        right = (do { l <- getPosition
                    ; sym "("; y <- simpleExpression; z <- oper; sym ")"
                    ; return (EAbs [(PVar (Nm("_x",l))
                                   ,EApp (EVar (Nm(z,l))) [y,EVar (Nm("_x",l))])])})
                                   
-- () (x) (x,y,z) (+) (+ 3) (3 *) (,) etc
parenExpression ::  MParser Expr
parenExpression = try section <|> tuple  
  where tuple = 
          do { xs <- parenS(sepBy expr (commA))
             ; return(expTuple xs)}                                
                                                   
                                     
-- [2,3,4]
listExpression :: MParser Expr
listExpression = (try empty) <|> (do { sym "["; x <- expr; tail x })
  where empty = do { sym "["; sym "]"; return (listExp [])}
        tail x = more x <|> count x <|> comp x
        more x = (sym "]" >> return(listExp [x])) <|>
                 (do { xs <- many (sym "," >> expr)
                     ; sym "]"
                     ; return(listExp (x:xs))})
        count i = do { sym ".."; j <- expr; sym "]"
                     ; return(EComp(Range i j))}
        comp x = do { sym "|"
                    ; xs <- sepBy bind (symboL ",")
                    ; sym "]"
                    ; return(EComp(Comp x xs))}
        bind = try gen <|> fmap Pred expr
        gen = do { p <- pattern
                 ; sym "<-"
                 ; e <- expr
                 ; return(Generator p e)}
                             
     
     
-- \ x -> f x
lambdaExpression :: MParser Expr
lambdaExpression =
    do{ resOp "\\"
      ; xs <- layout arm (return ())
      ; return(EAbs xs) }

arm:: MParser (Pat,Expr)
arm = do { pat <- pattern
         ; sym "->"
         ; e <- expr
         ; return(pat,e) }     

-- if x then y else z
ifExpression :: MParser Expr
ifExpression =
   do { keyword "if"
      ; e1 <- expr
      ; keyword "then"
      ; l1 <- getPosition
      ; e2 <- expr
      ; keyword "else"
      ; l2 <- getPosition
      ; e3 <- expr
      ; return(EApp (EAbs [(truePat,e2),(falsePat,e3)]) [e1])
      }
      
-- case e of { p -> body; ... }
caseExpression:: MParser Expr
caseExpression =
    do{ keyword "case"
      ; e <- expr
      ; keyword "of"
      ; alts <- layout arm (return ())
      ; return(EApp (EAbs alts) [e])
      }      


-------------------- Parsing Declarations ------------------------

d1 = parse2 decl "f x y z = x+ y"
d1b = parse2 (layoutDecl (return())) "f x y z = x+ y\nf a b c = 6"
d2 = parse2 decl "(x,y) = (5,f 6)"
d3 = parse2 decl "Cons x xs = [5,f 6]"
d4 = parse2 decl "(x,xs,C x ys) = (2,\"abc\",[5,f 6])"
d5 = parseFile decl "test1.funlog"
d6 = parseFile program "MapColoring.funlog"
d7 = parseFile program  "Soduko.funlog"
d8 = parse2 decl "dim x = [0,1,2,3]"
d9 = parseFile program "functions.funlog"

decl:: MParser Decl
decl =   dimP
     <|> datadec 
     <|> find 
     <|> dec
     <?> "decl"

extendEnum tynm cs f x = 
  case f x of
    Just s -> Just s
    Nothing -> case lookup x cs of
                 Just [] -> Just tynm
                 other -> Nothing

datadec:: MParser Decl
datadec = 
  do { pos <- getPosition
     ; keyword "data"
     ; nm <- conName
     ; sym "=" 
     ; cs <- sepBy constr (sym "|")
     ; (tabs) <- getState
     ; f <- get
     ; setState tabs
     ; put (extendEnum nm cs f)
     ; return(DataDec pos nm cs)}

constr:: MParser (String,[Maybe String])
constr = 
  do { s <- conName;
     ; xs <- many (fmap Just conName <|> (sym "_" >> return Nothing))
     ; return(s,xs) }

dimP :: MParser (Decl)
dimP = 
  do { pos <- getPosition
     ; keyword "dim"
     ; nm <- ident
     ; sym "#"
     ; t <- base
     ; sym "="
     ; e <- expr
     ; return(Domain pos t nm e)}

strategy = (sym "First" >> return First) <|> 
           (sym "Abstract" >> return Abstract) <|>
           (do { try (sym "Many"); e <- expr; return (Many e)})  <|> 
           (do { try (sym "Max"); e <- expr; return (Max e)})  <|> 
           (do { (sym "Min"); e <- expr; return (Min e)}) <?> 
           "strategy (First, Many, Max, Min, or Abstract)"


gg = do { p <- simplePattern; sym ":"; i <- initializer; return(p,i)}
tech = (try (sym "SAT") >> return SAT) <|>
       ((sym "SMT") >> return SMT) <|>
       ((sym "Narrowing") >> return Narrowing) <|>
       ((sym "IP") >> return IP)  
 
find :: MParser Decl
find =
  do { pos <- getPosition
     ; keyword "exists"
     ; pairs <- layout gg (return ()) -- (keyword "where")
     ; keyword "where"
     ; constraint <- expr
     ; keyword "find"
     ; st <- strategy
     ; keyword "by"
     ; t <- tech
     ; return(Find pos pairs constraint st t)
     }     

initializer = typeP <|> setP <|> arrayP <|> listP <|> fmap Ituple tupleP <|> domP
  where typeP = (sym "Int" >> return Iint) <|>
                (sym "Bool" >> return Ibool) <|>
                (sym "Double" >> return Idouble)
        setP = do { keyword "set"; i <- simpleExpression
                  ; x <- expr; sym ".."; y <- expr
                  ; return(Iset i x y)}
        tupleP = parenS(sepBy initializer (sym ","))
        domP = fmap Idomain expr
        listP = do { sym "List"
                   ; LInt n <- intLit
                   ; i <- initializer
                   ; return(Ilist n i)}
        arrayP = do { f <- array
                    ; e <- simpleExpression
                    ; i <- initializer
                    ; return(f e i)}
        array = ((sym "Array" <|> sym "Vector") >> return Iarray)  


---------------------------------------
-- All declarations in one parser
--  f = exp
-- f x y = exp
-- f(x) -> Int(x).
-- f(x) <- p(x),q(x).
-- f (x:xs) = e
-- f [] = e
-- Note they all start with
--  pat* -> ...
--  pat* <- ...
--  pat* =  ...

dec = do { pos <- getPosition
         ; lhs <- many1 simplePattern
         ; oper <- (arrow <|> larrow <|> (resOp "=" >> return "="))
         ; decRhs pos lhs oper }

decRhs pos lhs oper = 
  case (lhs,oper) of
    ([PVar f,arg@(PTuple TUPLE ps)],"->") ->  -- f(x,y,z) -> p(x),p(y),q(z).
       do { args <- pat2Names arg arg
          ; rhs <- ruleDeclare args
          ; return(Rule2 pos (name f) rhs)}
    ([PVar f,arg@(PTuple TUPLE ps)],"<-") ->  -- f(x,y,z) <- g(x,y), h(y,z).
       do { args <- pat2Names arg arg
          ; rhs <- ruleDefine args
          ; return(Rule2 pos (name f) rhs)}
    ([PVar f,PVar x],"->") ->                 -- f(x) <- g(x).
       do { rhs <- ruleDeclare [name x]
          ; return(Rule2 pos (name f) rhs)}
    ([PVar f,PVar x],"<-") ->                 -- f(x) -> p(x).
       do { rhs <- ruleDefine [name x]
          ; return(Rule2 pos (name f) rhs)}          
    ((PVar f : (p:ps)),"=") ->                -- f p1 p2 = e
       do { body <- expr; return(FunDec pos (name f) [(p:ps,body)]) }
    ([p],"=") ->                              -- x = e
       do { b <- expr; return(Def pos p b) }  
    ((PCon c []):ps,"=") ->                   -- C x y = e
       do { b<- expr; return(Def pos (PCon c ps) b)}
    (ps,oper) -> fail ("Illegal patterns to start value decl:" ++(show ps))
          
ruleDeclare args =  do { ps <- f args; sym "."; return(DeclRhs ps) }
  where f [x] = 
          do { z <- getPosition; p <- ident; nm <- parenS (exactly x "\nwhile parsing a rule declaration.")
             ; return[(nm,EVar (Nm(p,z)))]}
        f (x:xs) = 
          do { z <- getPosition; p <- ident; nm <- parenS (exactly x "\nwhile parsing a rule declaration.");
             ; sym ","; ws <- f xs; return((nm,EVar (Nm(p,z))):ws)}
             
ruleDefine args = 
  do { f <- formulas; sym "."; return(DefRhs args f)}


-----------------------------------------------------------------    

program = do { whiteSp; ds <- layoutDecl (return ()); eof; return (Prog ds)}

layoutDecl endtag = do { ds <- layout decl endtag
                       ; return(map merge (groupBy sameName ds)) }
  where sameName (FunDec p n _) (FunDec q m _) = n==m
        sameName (Rule2 _ n _) (Rule2 _ m _) = n==m
        sameName _ _ = False
        merge:: [Decl] -> Decl
        merge [d] = d
        merge ((FunDec pos name ws):ms) = FunDec pos name (ws++ concat(map g ms))
        merge [Rule2 pos nm (DeclRhs xs),Rule2 _ _ (DefRhs args form)] | consistent2 mess args xs =
                Rule2 pos nm (Both xs form)
          where mess = "\n"++show pos ++"\n"++nm++plistf id "(" args "," ")\n"
        g (FunDec pos name ws) = ws
       

consistent2 message [] [] = True
consistent2 message (x:xs) ((y,_):ys) = 
  if x==y then consistent2 message xs ys
          else error(message++"Rule declaration and rule definition, must have the same arguments in the same order.\n")
consistent2 message x y = error(message++"Rule declaration and rule definition, have a different number of arguments.\n")       

----------------------------
-- parsers for Typ 

base :: MParser Base
base = try(int <|> string <|> char <|> double <|> bool) <|> enum
  where int    = sym "Int" >> return Int
        string = sym "String" >> return String
        char   = sym "Char" >> return Char
        double = sym "Double" >> return Double
        bool   = sym "Bool" >> return Bool
        enum   = do { n <- conName; return(Enum n)}

{-

baseType :: MParser Typ
baseType = fmap toTyp base  

simpleTyp :: MParser Typ
simpleTyp = baseType <|> tycon <|> brack
  where brack  = do { ts <- parenS (sepBy typ (sym ","))
                    ; return(tupleT ts)}
        tycon = do { t <- conName; return(TyCon t Star) }
        
typ :: MParser Typ 
typ = (chainr1 apps (sym "->" >> return arrT))
  where arrows [t] = t
        arrows (t:ts) = arrT t (arrows ts)
        apps :: MParser Typ
        apps = fmap applyT (many1 simpleTyp)
-}

------------------- Parsing Formulas ---------------------------

pat2Names whole (PVar nm) = return [name nm]
pat2Names whole (PTuple TUPLE xs) = 
   do { ys <- mapM (pat2Names whole) xs; return(concat ys)}
pat2Names whole p = unexpected ("\nThe lhs of an embedded formula or quantified constraint can only contain names: "++show whole)

pats [p] = p
pats ps = PTuple TUPLE ps
                
atom = do { f <- atomName
          ; ps <- sepBy pattern commA
          ; lexemE(char ')')
          ; return(f,pats ps)}
          
atomic = esc <|> prim  
  where prim = do { pos <- getPosition; (name,p) <- atom; atomic pos p name } 
        atomic pos (PTuple TUPLE [p1,p2]) ('#':cs) = return(Equality p1 (Nm(cs,pos)) p2)
        atomic pos p name = return(Atomic(Prim (Nm(name,pos)) p))
        esc = do { pos <- getPosition; 
                 ; name <- escape 
                 ; ps <- parenS(sepBy pattern commA)
                 ; return(Atomic(Escape pos name (pats ps)))}
                 
escape = do { sym "$"; exp <|> var }
  where var = do { pos <- getPosition
                 ; nm <- ident
                 ; return(EVar (Nm(nm,pos)))}
        exp = do { e <- parenS expr
                 ; return e }                     



simpleFormula = projection <|> parenFormula <|> atomic <|> equality

-- We need to fix this.
-- this parses,  "x =#str $y"
-- but this doesn't, "$x =#str y"
equality = 
  do { p1 <- pattern
     ; sym "=#"
     ; n <- nameP
     ; p2 <- pattern
     ; return(Equality p1 n p2) }
                
parenFormula = parenS(formulas)
  
projection = braceS local  
  where local = do { pos <- getPosition
                   ; p <- pattern
                   ; larrow
                   ; rhs <- formulas
                   ; args <- pat2Names p p
                   ; return(Embed args rhs)}   
  
formulas = disjs 
  where semi = char ';' >> return Disj
        comma = char ',' >> return Conj
        conjs = chainl1 one (lexemE comma)
        disjs = chainl1 conjs (lexemE semi)
        one = do { negf <- neg
                 ; z <- simpleFormula 
                 ; return(negf z) }

neg = (lexemE( char '!') >> return Negation) <|> return id

formulaP = formulas <?> "formula"

-- formula                    
t14 = parse2 (formulas) "father(x,y), (h(x)) ; !(k(2))"
t14a = parse2 (formulas) "(father(x,y), h(x)) ; !k(2)"
t14b = parse2 (formulas) "father(x,y), (h(x) ; !k(2))"
t15 = parse2 (many (formulaP)) "g(x) father(x,y)"
t16 = parse2 (formulaP) "(grand(x,z) <- parent(x,y), parent(y,z))"

p1 = parse2 formulas "parent(x,y),parent(y,z)"



------------------------ Parsing constraints -----------------

c1 = parse2 constraint "f(x,y),g(y,z) | (x,y) -> (z)"
c2 = parse2 constraint "one g(x,y,z)"
c3 = parse2 constraint "g(x,y,z),h(y) <= j(x); k(q)"


{-
conOrForm = (keywordConstraint >>= (return . Right)) <|>
            (do { x <- formulaP; next x })
  where next x = (conTail x >>= (return . Right)) <|> return(Left x)
-}

constraint = 
   fmap or (sepBy1 
   (fmap and (sepBy1 equal_constraint (sym "&&")))
                   (sym "||"))
  where and [x] = x
        and xs = BoolC "&&" xs
        or [x] = x
        or xs = BoolC "||" xs

equal_constraint = 
  do { c1 <- simple_constraint
     ; f <- try(do { sym "<=>"
                   ; c2 <- simple_constraint
                   ; return(\ c1 -> BoolC "<=>" [c1,c2])}) <|> (return id)
     ; return(f c1) }
     
simple_constraint = (parenS constraint) <|> keywordConstraint <|> formPrefixConstraint <|> allC
  where keywordConstraint = do { f <- constraintKeyword; x <- formulaP; return(f x) }
        formPrefixConstraint = do { x <- formulaP; conTail x }
        constraintKeyword = 
           (keyword "one" >> return One)   <|>
           (keyword "none" >> return None) <|>
           (keyword "some" >> return Some) <|>
           (keyword "full" >> return Full)   
        conTail x = 
           (do { sym "->"; y <- formulaP; return(RightArrow x y)}) <|>
           (do { sym "<="; y<- formulaP; return(Subset x y)}) <|>
           (do { sym "=="; y<- formulaP; return(BoolC "&&" [Subset x y,Subset y x])}) <|>
           (do { sym "|"
               ; dom <- names
               ; sym "->"
               ; rng <- names
               ; return(FunDep x dom rng)}) <|>
           (return(Fact x))    

allC = do{ keyword "forall"
         ; p <- pattern
         ; args <- pat2Names p p
         ; sym "<-"
         ; f <- formulaP
         ; sym "."
         ; c <- constraint
         ; return(All args f c)}

conOrForm = do { x <- constraint; test x } where
  test x = (do { sym "."; return(Right x)}) <|>
           (case x of
              Fact y -> return(Left y)
              other -> return(Right x)) -- unexpected ("Missing dot after\n  "++show x))

-- const = keyPrefix

names = (parenS (sepBy ident commA)) <|>
        (do { p <- ident; return [p]})


