
structure TypeCheck = struct 

exception NotImplementedYet of string;

open ProgramTypes;

fun fst(x,y) = x;
fun snd(x,y) = y;

(* *********** Type equality and subtyping ********************* *)

fun basiceq (x,y) =
case (x,y) of
  (Real,Real) => true
| (Int,Int) => true
| (Bool,Bool) => true
| (_,_) => false

fun typeEqual (x,y) =
case (x,y) of
  (BasicType x,BasicType y) => basiceq(x,y)
| (ArrayType x,ArrayType y) => basiceq(x,y)
| (ObjType x,ObjType y) => x=y
| (VoidType,VoidType) => true
| (_,_) => false

val boolT = BasicType Bool
val intT = BasicType Int
val realT = BasicType Real;

fun showB Int = "int"
  | showB Real = "real"
  | showB Bool = "bool";

fun showt (ArrayType x) = showB x^" []"
  | showt (ObjType s) = s
  | showt (BasicType x) = showB x
  | showt VoidType = "void"
  
(* ************** Handling errors ****************** *)

exception NotFound of string;
exception NoClass of (string*Exp);
exception TypeError of Exp*string;

fun error e s = raise(TypeError (e,s));
 
fun check exp typ expected result = 
  if typeEqual(typ,expected)
     then result
     else error exp ("Found type "^(showt typ)^" expecting type "^(showt expected));
      

(* ******* The class table is an n-way branching tree ****** *) 

datatype CTab
   = Node of
        Id *                            (* Class Name      *)
        (Id * Type)list *               (* Class Variables *)
        (Id * Type list * Type)list *   (* Class Methods   *)
        CTab ref *                      (* Parent Class    *)
        CTab list                       (* Sub Classes     *)
   | NullClass;
   

fun clName (Node(nm,vs,ms,p,subs)) = nm;
fun clVars (Node(nm,vs,ms,p,subs)) = vs;
fun clParent (Node(nm,vs,ms,ref p,subs)) = p;


fun lookupVar class env s =  
  case List.find (fn (x,t) => x=s) env of
    SOME(x,t) => (t,Var((0,0),s))
  | NONE => (case class of
               NullClass => raise (NotFound s)
             | _ => lookupVar (clParent class) (clVars class) s)

  
(* classHasMethod: CTab -> Id -> Id * Type list * Type   *)
fun classHasMethod classTable name = raise (NotImplementedYet "classHasMethod")
  

val root = Node("object",[],[],ref NullClass,[]);


fun newClass name vars methods parent NullClass = NullClass
  | newClass name vars methods parent (n as Node(nm,vs,ms,p,subs)) =
     if parent=nm
        then let val p1 = ref n
                 val new = Node(name,vars,methods,p1,[])
                 val newP = Node(nm,vs,ms,p,new::subs)
                 val _ = p1 := newP
             in newP end
        else Node(nm,vs,ms,p,map (newClass name vars methods parent) subs)


fun classDefined exp c NullClass = raise(NoClass (c,exp))
  | classDefined exp c (cl as Node(n,vs,ms,p,ss)) = 
      let fun search [] = raise(NoClass (c,exp))
            | search (x::xs) = ((classDefined exp c x) handle 
                                 (NoClass _) => search xs)
      in if n=c then cl else search ss end

fun defines name NullClass = false
  | defines name (Node(n,vs,ms,p,ss)) =
      if name=n then true else List.exists (defines name) ss;

fun treeSubtype NullClass (x,y) = false
  | treeSubtype (Node(nm,vs,ms,p,ss)) (x,y) =
     if nm = y
        then List.exists (defines x) ss
        else List.exists (fn t => treeSubtype t (x,y)) ss

fun subtype classH (x,y) =
  case (x,y) of
    (x,ObjType "object") => true
  | (ObjType x,ObjType y) => treeSubtype classH (x,y)
  | (BasicType Int,BasicType Real) => true
  | (_,_) => typeEqual(x,y)


fun suberror exp x y = 
  error exp (" has type: "^showt x^" which is not a subtype of: "^showt y);
  
fun subsumes exp classH [] [] = []
  | subsumes exp classH [] xs = error exp "Too few arguments"
  | subsumes exp classH xs [] = error exp "Too many arguments"
  | subsumes term classH ((actualtype,exp)::es) (needtype::ts) =
       if subtype classH (actualtype,needtype)
          then exp :: subsumes term classH es ts
          else suberror exp actualtype needtype
    
  
  
(* ******************************************************* *)
(* **** Extracting only type info from declarations ****** *)

fun varPair (VarDecl(loc,typ,name,body)) = (name,typ);

fun formalPair (Formal(typ,name)) = (name,typ);

fun methodTriple (MetDecl(loc,rng,name,fs,vs,ss)) = 
  let fun formal (Formal(typ,name)) = typ
  in (name,map formal fs,rng) end;


(* ******** environments and their operations ********* *)

datatype Env = E of (Type option * CTab * CTab * (Id * Type) list)

fun R  (E(r,th,c,te)) = r;
fun Th (E(r,th,c,te)) = th;
fun C  (E(r,th,c,te)) = c;
fun TE (E(r,th,c,te)) = te;

fun extendTE pairs (E(r,th,c,te)) = E(r,th,c,pairs @ te);

fun setReturn VoidType (E(r,th,c,te)) = E(NONE,th,c,te)
  | setReturn rng (E(r,th,c,te)) = E(SOME rng,th,c,te);

fun thisType env = ObjType(clName (Th env));

fun extClass (ClassDec(loc,name,super,vs,ms))  (E(r,th,c,te)) =
 let val c2 = newClass name (map varPair vs) (map methodTriple ms) super c
     val this = classDefined (Var ((0,0),"looking for installed class")) name c2
 in E(r,this,c2,te) end


(* *********************************************************** *)
(* **** Handling Coercions from int to real in binary ops **** *)

fun binerr oper xt yt x y = 
 error (Binop(oper,x,y)) (" does not match ("^showB xt^" * "^showB yt^")")

(* coerce: BINOP -> Basic -> Basic -> Exp -> Exp -> Type * Exp  *)

fun coerce ADD Int Real x y = (BasicType Real,Binop(ADD,Coerce x,y))
  | coerce oper xtype ytype xterm yterm = raise(NotImplementedYet "coerce")  


(* coerceRel: RELOP -> Basic -> Basic -> Exp -> Exp -> Type * Exp  *)  
fun coerceRel Eq Bool Bool x y = (BasicType Bool,Relop(Eq,x,y))
  | coerceRel oper xtype ytype xterm yterm = raise(NotImplementedYet "coerceRel")    

(* ************************************************************* *)
(* Type checking functions for each of the abstract syntax trees *)

fun TCConstant (Cint _) = BasicType Int
  | TCConstant (Creal _) = BasicType Real
  | TCConstant (Cbool _) = BasicType Bool;


fun TCExp exp (env:Env) = 
case exp of
  Var(loc,"this") => (thisType env,exp)
| Var(loc,s) => lookupVar (Th env) (TE env) s
| Literal(loc,c) => (TCConstant c,exp)
| Binop(oper,x,y) =>
    let val (xt,x2) = TCExp x env
        val (yt,y2) = TCExp y env
    in case (xt,yt) of
         (BasicType xb,BasicType yb) => coerce oper xb yb x2 y2
       | _ => error exp "Non basic type in infix operation"
    end
| Call(obj,x,_,args) => 
    (case TCExp obj env of
       (ObjType c,obj2) => 
          let val class = classDefined exp c (C env)
              val (defclass,ts,t) = classHasMethod class x
              val pairs = List.map (fn x => TCExp x env) args
              val args2 = subsumes exp (C env) pairs ts
          in (t,Call(obj2,x,SOME defclass,args2)) end
     | (z,_) => error exp (" does not have an object type: "^showt z))
| _ => raise (NotImplementedYet "TCExp")


fun TCExpOption NONE env = NONE
  | TCExpOption (SOME x) env = SOME(TCExp x env);

fun TCStmt stmt env =
 case stmt of
   If(test,x,y) => 
      let val (testt,test2) = TCExp test env
          val x2 = TCStmt x env
          val y2 = TCStmt y env
      in check test testt boolT (If(test2,x2,y2)) end
 | Block ss => Block(map (fn s => TCStmt s env) ss)
 | _ => raise (NotImplementedYet "TCStmt")


fun TCVarDecl (d as VarDecl(loc,typ,name,NONE)) env = (d,extendTE [(name,typ)] env)
  | TCVarDecl (VarDecl(loc,typ,name,SOME body)) env = 
  let val (bodyt,body2) = TCExp body env
      val decl2 = VarDecl(loc,typ,name,SOME body2)
      val env2 = extendTE [(name,typ)] env
  in check body bodyt typ (decl2,env2) end;
  
fun TCVarDecls [] env = ([],env)
  | TCVarDecls (d::ds) env = 
     let val (d2,env2) = TCVarDecl d env
         val (ds2,env3) = TCVarDecls ds env2
     in (d2::ds2,env3) end;
     

(* TCMethod assumes the type of all the mutually recursive *)
(* methods have already been added to the environment.     *)

fun TCMethod (MetDecl(loc,rng,name,fs,vs,ss)) env = 
  let val env2 = extendTE (map formalPair fs) env
      val (vs2,env3) = TCVarDecls vs env2
      val env4 = setReturn rng env3
      val ss2 = map (fn s => TCStmt s env4) ss
  in MetDecl(loc,rng,name,fs,vs2,ss2) end;


fun TCClass (cl as ClassDec(loc,name,super,vs,ms)) env = 
  let val (vs2,env2) = TCVarDecls vs env
      val env3 = extClass cl env2  (* Adds method info *)
      val ms2 = map (fn m => TCMethod m env3) ms
  in (ClassDec(loc,name,super,vs2,ms2),env3) end;

fun TCClassList [] env = ([],env)
  | TCClassList (c::cs) env = 
    let val (c2,env2) = TCClass c env
        val (cs2,env3) = TCClassList cs env2
    in (c2::cs2,env3) end;


val empty = E(NONE,NullClass,NullClass,[]);

fun TCProgram (Program cs) = 
   (TCClassList cs (E(NONE,NullClass,root,[]))) handle
     NotFound s => (print s; ([],empty))
   | NoClass (s,e) => (print s; ([],empty))
   | TypeError(e,s) => (print s; ([],empty))

      
end