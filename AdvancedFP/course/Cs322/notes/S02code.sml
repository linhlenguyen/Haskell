
structure Arith1 = struct

open ProgramTypes

type Reg = int;

datatype IR 
  = LoadI of (string * Reg)
  | LoadAO of (Reg * Reg * Reg)
  | Arith of (BINOP * Reg * Reg * Reg);
  
fun showOp ADD = "Add "
  | showOp SUB = "Sub "
  | showOp MUL = "Mul "
  | showOp DIV = "Div "
  | showOp AND = "And "
  | showOp OR  = "Or  "

fun showRel EQ  = "Eq  "
  | showRel NE  = "Ne  "
  | showRel LT  = "Lt  "
  | showRel LE  = "Le  "
  | showRel GT  = "Gt  "
  | showRel GE  = "Ge  "  

fun showReg 0 = "rA"
  | showReg n = "r"^Int.toString n;

fun showIR (LoadI(s,r)) = 
      "loadI  "^s^"    => "^showReg r
 |  showIR (LoadAO(x,y,z)) = 
      "loadAO "^showReg x^","^showReg y^" => "^
      showReg z
 |  showIR (Arith(m,x,y,z)) = 
      showOp m^"   "^showReg x^","^
      showReg y^" => "^showReg z;
 
(* ******************************************************* *)

val emitted = ref ([]: IR list);
fun emit x = emitted := (x :: (!emitted));

val Rarp = 0;
val firstRegister = 1;
val regCount = ref firstRegister;

fun resetRegister () = regCount := firstRegister;


fun NextRegister() = 
  let val n = !regCount 
  in (regCount := n+1; n) end;

fun offset (Var(_,x)) = 
  let val result = NextRegister()
  in emit (LoadI("@"^x,result)); 
     result 
  end;
  
fun base x = Rarp;

fun expr node =
case node of
  Var(_,x) => 
     let val t1 = base node
         val t2 = offset node
         val result = NextRegister()
     in emit (LoadAO(t1,t2,result)); 
        result 
     end
| Binop(m,x,y) =>
     let val t1 = expr x
         val t2 = expr y
         val result = NextRegister()
     in emit (Arith(m,t1,t2,result));
        result 
     end
| Literal(_,Cint n) =>   
     let val result = NextRegister()
     in emit (LoadI(n,result)); 
        result 
     end



fun trans x =
 let val _ = emitted := []
     val _ = resetRegister () 
     val result = expr x
     val instrs = rev (!emitted)
     val _ = print "\n";
     fun sh x = print(showIR x^"\n")
     val _ = map sh instrs
     val _ = print "\n\n";
 in instrs end;

(* ************************************************* *)
(* *** examples to test on ************************* *)

fun var x = (Var((0,0),x));
fun const n = Literal((0,0),Cint(Int.toString n));

val ex1 = Binop(SUB,var "x",Binop(MUL,const 2,var "y"));

val ex2 = Binop(SUB,var "x",Binop(MUL,const 2,var "x"));

(* ***************** Variable dictionaries *************** *)



fun expr2 dict node =
case node of
  Var(_,x) => 
     (case List.find (fn (nm,r) => x=nm) (!dict) of
        NONE => let val t1 = base node
                    val t2 = offset node
                    val result = NextRegister()
                in emit (LoadAO(t1,t2,result));
                   dict := (x,result)::(!dict);
                   result 
                end
     | SOME(x,r) => r)
| Binop(m,x,y) =>
     let val t1 = expr2 dict x
         val t2 = expr2 dict y
         val result = NextRegister()
     in emit (Arith(m,t1,t2,result));
        result 
     end
| Literal(_,Cint n) =>   
     let val result = NextRegister()
     in emit (LoadI(n,result)); 
        result 
     end

fun trans2 x =
 let val _ = emitted := []
     val _ = resetRegister () 
     val dict = ref []
     val result = expr2 dict x
     val instrs = rev (!emitted)
     val _ = print "\n";
     fun sh x = print(showIR x^"\n")
     val _ = map sh instrs
     val _ = print "\n\n";
 in instrs end;


end