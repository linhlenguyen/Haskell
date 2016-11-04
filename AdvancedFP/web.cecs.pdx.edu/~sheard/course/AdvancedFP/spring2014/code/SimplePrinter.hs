module SimplePrinter  where
 

-- import the Hughes library qualified 
import qualified Text.PrettyPrint.HughesPJ as PP
-- import a few widely used operations without qualification
import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($$),($+$),render) 


-----------------------------
-- These are the data from the simple imperative language

type Name = String
type Op = String

data Exp
  = Var Name 
  | Int Int
  | Bool Bool
  | Oper Exp Op Exp
  
         
data Stmt
  = Assign Name Exp
  | While Exp Stmt
  | If Exp Stmt Stmt
  | Call Name [Exp]
  | Begin [Decl] [Stmt]
 
 
data Decl
  = Val Name Exp
  | Fun Name [Name] Stmt
 

ppDecl:: Decl -> Doc
ppDecl (Val n e) = PP.sep [text "val", text n, text "=",ppExp e]
ppDecl (Fun f xs s) = 
   PP.sep [PP.sep[text "fun", text f
                 ,ppArgs text xs,  text "="]
          ,PP.nest 2 (ppStmt s)]
          
 
ppExp:: Exp -> Doc
ppExp (Var s) = text s
ppExp (Int n) = text(show n)
ppExp (Bool b) = text(show b)
ppExp (Oper e1 x e2) = 
  PP.parens(PP.sep [ppExp e1,text x,ppExp e2])
  
ppStmt:: Stmt -> Doc
ppStmt (Assign n e) = 
  PP.sep[text n, text ":=", ppExp e]
ppStmt (While e s) = 
  PP.sep[PP.sep [text "while", ppExp e],ppStmt s]
ppStmt (If e s1 s2) =
  PP.sep[PP.sep [ text "if", ppExp e]
        ,PP.nest 3 (ppStmt s1)
        ,PP.nest 3 (ppStmt s2)]
ppStmt (Call nm es) =
  PP.sep[text "call", text nm
        ,ppArgs ppExp es]
ppStmt (Begin ds es) = 
  PP.sep[text "begin"
        ,PP.nest 2 (PP.vcat (map ppDec ds ++ (text "  " : map ppSt es)))
        , text "end"]

ppDec d = ppDecl d <> text ";"
ppSt d = ppStmt d <> text ";"

ppArgs f es = PP.parens(PP.sep (PP.punctuate (text ",") (map f es)))

 
instance Show Exp where
  show x = render(ppExp x)
  
instance Show Decl where
  show x = render(ppDecl x)
  
instance Show Stmt where
  show x = render(ppStmt x)  
  
s1 = Begin [Val "x" (Int 5),Fun "f" ["x","y"] (Assign "y" (Int 4))] [Call "f" [Int 4,Int 5]]

s2 = Begin [Fun "g" ["a","b","c"] s1] [Call "g" [Int 4, Int 7, Int 8]]  