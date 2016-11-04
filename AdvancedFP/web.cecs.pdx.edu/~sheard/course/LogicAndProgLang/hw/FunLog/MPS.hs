module MPS -- (MPS(..),OP(..),BndRng(..),Row(..),Number(..),showMPS,printMPS)
   where

import Text.ParserCombinators.Parsec  
import Text.ParserCombinators.Parsec.Token
import qualified System.IO.Error


data MPS num
  = NAME String
  | ROWS [ Row ]
  | COLUMNS [(Var,[(RowName,num)])]
  | RHS String [(RowName,num)]
  | BOUNDS String [(BndRng,Var,num)]
  | ENDATA

data OP = N | E | L | G  
type Var = String
type RowName = String
data BndRng = UP | LO | FX | FR 
data Row = Row OP RowName

-- data Number = I Int | R Double
-- instance Show Number where
--   show (I x) = show x
--   show (R x) = show x

----------------------------------------------------

display:: Show num => MPS num -> (String,[[String]])
display (NAME s) = ("NAME          "++s,[])
display (ROWS rs) = ("ROWS",map f rs)
  where f (Row x y) = [show x,y]
display (COLUMNS xs) = ("COLUMNS",concat (map f xs))
  where f (v,cs) = (map (\(nm,n) -> ["",v,nm,show n]) cs)
display (RHS s xs) = ("RHS",map f xs)
  where f (nm,n) = ["",s,nm,show n]
display (BOUNDS s xs) = ("BOUNDS",map f xs)
  where f (rng,v,n) = [show rng,s,v,show n]
display (ENDATA) = ("ENDATA",[])



instance Show OP where
  show N = " N"    -- the preceeding spaces are important
  show E = " E"
  show L = " L"
  show G = " G"

instance Show BndRng where
  show UP = " UP"    -- the preceeding spaces are important
  show LO = " LO"
  show FX = " FX"
  show FR = " FR"  

colStarts = [1, 5, 15, 25, 40, 50]
colPads = [Right 4,Right 10,Right 10,Left 12,Right 10,Left 12]

pad :: Either Int Int -> String -> String
pad (Left n) xs | length xs > n = error ("String "++show xs++" to long to pad to length "++show n)
pad (Right n) xs | length xs > n = error ("String "++show xs++" to long to pad to length "++show n)
pad (Left n) xs = replicate (n - length xs) ' ' ++ xs
pad (Right n) xs = xs ++ replicate (n - length xs) ' '

fillC ns [] = ""
fillC [] (s:ss) = error ("Too many columns (more than 6)")
fillC (n:ns) (s:ss) = pad n s ++ (fillC ns ss)

showMPS :: Show num => [MPS num] -> [String]
showMPS [] = []
showMPS (x:xs) = tag : (map (fillC colPads) rs)++ showMPS xs
 where (tag,rs) = display x

printMPS xs = putStrLn(unlines (showMPS xs))

test:: [MPS Int]
test =
  [ NAME "TESTPROB"
  ,ROWS [Row N "COST"
        ,Row L "LIM1"
        ,Row G "LIM2"
        ,Row E "MYEQN" ]
  ,COLUMNS [("XONE",[("COST", 1),("LIM1", 1),("LIM2", 1)])
           ,("YTWO",[("COST", 4),("LIM1", 1),("MYEQN", (-1))])
           ,("ZTHREE",[("COST", 9),("LIM2", 1),("MYEQN", 1)])
           ]
  ,RHS "RHS1" [("LIM1", 5),("LIM2", 10),("MYEQN", 7)]       
  ,BOUNDS "BND1" [(UP,"XONE", 4),(LO,"YTWO", (-1)),(UP,"YTWO",  1)] 
  ,ENDATA
  ]
  
---------------------------------------------------

--------------------------------------------
-- Parsing the output of the CLP solver

clpLine = 
  do { count <- intLit
     ; var <- ident
     ; value <- intLit
     ; flush <- many (noneOf "\n")
     ; whiteSpace mps
     ; return(var,value)}
    
clp = many clpLine    

clpFile file =
  do {  possible <- System.IO.Error.tryIOError (readFile file)
     ; case possible of
         Right contents -> 
            case runParser (whiteSpace mps >> clp) () file contents of
              Right ans -> return ans
              Left message -> error(show message)
         Left err -> error(show err) }



emptyDef    = LanguageDef 
               { commentStart   = ""
               , commentEnd     = ""
               , commentLine    = ""
               , nestedComments = True
               , identStart     = letter <|> char '_'
               , identLetter    = alphaNum <|> oneOf "_'"
               , opStart        = opLetter emptyDef
               , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , reservedOpNames= []
               , reservedNames  = []
               , caseSensitive  = True
               }
               
mps = makeTokenParser emptyDef

intLit :: CharParser st Int
intLit = fmap fromInteger (integer mps)

ident = identifier mps
                
  
  