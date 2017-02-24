
structure ProgramTypes = struct

exception LexicalError of string * (int * int);
exception ParseError of string * int;

type Id = string;

(** Representing types for mini-Java **)

type loc = (int * int)

datatype Basic = Bool | Int | Real;

datatype Type 
  = BasicType of Basic
  | ArrayType of Basic
  | ObjType of Id
  | VoidType;


(* A slot of type (`x TC) is an option *)
(* type. The parser places NONE there  *)
(* and the type-checker fills it in    *)
(* with (SOME x) when "x" is known     *)

type 'x TC = 'x option;

(******** Representing Programs *******)


datatype BINOP = ADD | SUB | MUL | DIV        (* Arithmetic *)
               | AND | OR;
datatype RELOP =  EQ | NE | LT | LE | GT | GE; 


fun evalBINOP binop x y = case binop of
  ADD => x+y
| SUB => x-y
| MUL => x*y
| DIV => x*y
(* Are AND and OR supposed to be bitwise operations?  
   If so, these definitions are wrong.
   Here, we assume positive represents true and zero represents false.
 *)
| AND => x*y	(* n*n > 0  and  n*0, 0*0, 0*n = 0  (for n>0) *)
| OR  => x+y	(* 0+0 = 0  and  n+n, n+0, 0+n > 0  (for n>0) *)

fun evalRELOP relop x y = case relop of
  EQ => (x =  y)
| NE => (x <> y)
| LT => (x <  y)
| LE => (x <= y)
| GT => (x >  y)
| GE => (x >= y)

datatype Constant         (* Literal constants *)
  = Cint of string
  | Creal of string
  | Cbool of bool;

datatype Exp
  = Literal of loc * Constant           (* 5, 6.3, true *)    
  | Binop of BINOP * Exp * Exp          (* x + 3        *)
  | Relop of RELOP * Exp * Exp          (* x < 7.7      *)
  | Not of Exp                          (* ! x          *)
  | ArrayElm of Exp * Exp * (Basic TC)  (* x[3]         *)
  | ArrayLen of Exp                     (* x.length()   *)
  | Call of Exp * Id * Id TC * Exp list (* x.f(1,z)     *)
  | NewArray of Basic * Exp             (* new int[3]   *)
  | NewObject of loc*Id                 (* new point()  *)
  (* Coerce is used only in type checking               *)
  | Coerce of Exp 
  | Member of loc * Exp * Id * Id
  | Var of loc * Id
  | This of loc
 

datatype Stmt 
  = Block of Stmt list       (* {x:5; print(2)}           *)
  | Assign of Exp option * Id * Exp option * Exp  
                             (* p.x[2]=5  p.x=5  x=5      *)
  | CallStmt of Exp * Id * Id TC * Exp list       
                             (* x.f(1,z)                  *)             
  | If of Exp * Stmt * Stmt  (* if (p<2) x=5 else x=6     *)
  | While of Exp * Stmt      (* while (p) s               *)
  | PrintE of Exp            (* System.out.println(x)     *)
  | PrintT of string         (* System.out.println("zbc") *)
  | Return of Exp option;    (* return (x+3)              *)           

datatype VarDecl = VarDecl of loc * Type * Id * Exp option;
  
datatype Formal = Formal of Type * Id;

datatype MetDecl = MetDecl of loc * Type * Id * 
                   Formal list * VarDecl list * 
                   Stmt list;

datatype ClassDec 
  = ClassDec of loc * Id * Id * VarDecl list * MetDecl list;
  
  
datatype Program = Program of ClassDec list;  

(* *** Finding the location in the source file of some code *** *)

fun or (SOME x) _ = SOME x
  | or NONE x = x;
fun orL [] = NONE
  | orL (x::xs) = or x (orL xs)

fun posE exp =
  case exp of
    Literal (pos,x)       => SOME pos
  | Binop (oper,x,y)      => or (posE x) (posE y)  
  | Relop (oper,x,y)      => or (posE x) (posE y)  
  | Not x                 => posE x
  | ArrayElm (x,n,NONE)   => or (posE x) (posE n)
  | ArrayElm (x,n,SOME t) => or (posE x) (posE n)
  | ArrayLen x            => posE x
  | Call (e,f,_,xs)       => or (posE e) (orL (map posE xs))
  | NewArray (t,x)        => posE x
  | NewObject(pos,x)      => SOME pos
  | Coerce e              => posE e
  | Var(pos,x)            => SOME pos
  | Member(pos,_,_,x)      => SOME pos
  | This pos              => SOME pos

fun posS stmt =
   case stmt of
     Block ss => orL (map posS ss)
   | Assign (SOME e, x, SOME n, v) => or (posE e) (or (posE n) (posE v))
   | Assign (SOME e, x, NONE, v) => or (posE e) (posE v)
   | Assign (NONE, x, SOME n, v) => or (posE n) (posE v)
   | Assign (NONE, x, NONE, v) => posE v
   | CallStmt (e,f,_,xs) =>  or (posE e) (orL (map posE xs))
   | If (g,x,y) => or (posE g) (or (posS x) (posS y))
   | While (g,b) => or (posE g) (posS b)
   | PrintE e => posE e
   | PrintT t => NONE
   | Return NONE => NONE
   | Return (SOME e) => posE e;
   
fun at NONE = ""
  | at (SOME(x,y)) = "At line: "^(Int.toString x)^" column: "^(Int.toString y)^"\n"
  

fun isBasic (BasicType _) = true
  | isBasic _             = false

val boolT = BasicType Bool
val intT = BasicType Int
val realT = BasicType Real;

fun plistf f sep [] = ""
  | plistf f sep [x] = f x
  | plistf f sep (x::xs) = f x ^ sep ^ plistf f sep xs

fun showB Int = "int"
  | showB Real = "real"
  | showB Bool = "boolean";

fun showT (ArrayType x) = showB x^" []"
  | showT (ObjType s) = s
  | showT (BasicType x) = showB x
  | showT VoidType = "void"

fun showTp (x as (ArrayType _)) = "(" ^ (showT x) ^ ")"
  | showTp x = showT x;
  
fun showConstant (Cint i) =  i
  | showConstant (Creal r) = r
  | showConstant (Cbool b) = Bool.toString b

fun showOp ADD = " + "
  | showOp SUB = " - "
  | showOp MUL = " * "
  | showOp DIV = " / "
  | showOp AND = " && "
  | showOp OR  = " || "
  
fun showRel EQ  = " == "
  | showRel NE  = " != "
  | showRel LT  = " < "
  | showRel LE  = " <= "
  | showRel GT  = " > "
  | showRel GE  = " >= "

fun showPar (Literal(pos,x)) = showConstant x
  | showPar (Var (pos,x)) = x
  | showPar (Member(pos,e, cid, x)) = showPar e ^ "." ^ x
  | showPar (This pos) = "this"
  | showPar e = "("^ showExp e ^")"

and showExp exp =
  case exp of
    Literal (pos,x)       => showConstant x
  | Binop (oper,x,y)      => showPar x ^ showOp oper ^ showPar y
  | Relop (oper,x,y)      => showPar x ^ showRel oper ^ showPar y
  | Not x                 => "!" ^ showPar x
  | ArrayElm (x,n,NONE)   => showPar x ^"["^ showExp n ^"]"
  | ArrayElm (x,n,SOME t) => showB t ^" "^ showPar x ^"["^ showExp n ^"]"
  | ArrayLen x            => showPar x ^".length()"
  | Call (e,f,_,xs)       => showPar e ^"."^ f ^"("^(plistf showExp "," xs)^")"
  | NewArray (t,x)        => "new "^ showB t ^" "^ showPar x
  | NewObject(pos,x)      => "new " ^ x ^ "()"
  | Coerce e              => showExp e
  | Var(pos, x)         => x
  | Member(pos,e,cid,x)       => showPar e ^"."^ x
  | This pos                 => "this"

fun showStmt stmt =
  (case stmt of
     Block ss =>
       "{\n"^ (String.concat (map (fn s => "    "^ showStmt s) ss)) ^"}\n"
   | Assign (SOME e, x, SOME n, v) =>
       showPar e ^"."^ x ^"[" ^ showExp n ^"] = "^ showExp v ^";"
   | Assign (SOME e, x, NONE, v) =>
       showPar e ^"."^ x ^" = "^ showExp v ^";"
   | Assign (NONE, x, SOME n, v) =>
       x ^"[" ^ showExp n ^"] = "^ showExp v ^";"
   | Assign (NONE, x, NONE, v) =>
       x ^" = "^ showExp v ^";"
   | CallStmt (e,f,_,xs) => showPar e ^"."^ f ^"("^
                         (plistf showExp "," xs)^");"
   | If (g,x,y) => "if ("^ showExp g ^")\n"^ showStmt x ^"else"^ showStmt y
   | While (g,b) => "while ("^ showExp g ^")\n"^ showStmt b
   | PrintE e => "System.out.println("^ showExp e ^");"
   | PrintT t => "System.out.println(\""^ t ^"\");"
   | Return NONE => "return;"
   | Return (SOME e) => "return "^ showPar e ^";"
  )^"\n"



end (* ProgramTypes *)
