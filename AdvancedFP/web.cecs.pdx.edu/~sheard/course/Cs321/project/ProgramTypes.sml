
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
  | Assign of (Exp*string) option * Id * (Exp*Basic) option * Exp  
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


end (* ProgramTypes *)
