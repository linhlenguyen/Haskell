
structure Expr2 = struct

open ProgramTypes

type Reg = int;
type Label = int;
type CC = int;


datatype IR 
  = LoadI of (string * Reg)
  | LoadAO of (Reg * Reg * Reg)
  | Arith of (BINOP * Reg * Reg * Reg)

  | Comp of (Reg * Reg * CC)
  | Neg of (Reg * Reg)
  | Cmp of (RELOP * Reg * Reg * Reg)
  | Cbr of (RELOP * CC * Label * Label)
  | JumpI of Label
  | Lab of (Label * IR)
  | Nop;
  
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

fun showLab n = "L"^Int.toString n; 
fun showCC n = "cc"^Int.toString n;  

fun showIR (LoadI(s,r)) = 
      "loadI   "^s^"    => "^showReg r
 |  showIR (LoadAO(x,y,z)) = 
      "loadAO  "^showReg x^","^showReg y^" => "^
      showReg z
 |  showIR (Arith(m,x,y,z)) = 
      showOp m^"    "^showReg x^","^
      showReg y^" => "^showReg z
      
 |  showIR (Neg(x,y)) = 
      "not     "^showReg x^" => "^
      showReg y        
 |  showIR (Comp(x,y,z)) = 
      "comp    "^showReg x^","^showReg y^" => "^
      showCC z    
 |  showIR (Cmp(m,x,y,z)) = 
      "cmp_"^showRel m^" "^showReg x^","^
      showReg y^" => "^showReg z
 |  showIR (Cbr(m,cc,l1,l2)) = 
      "cbr_"^showRel m^"  "^showCC cc^"   -> "^
      showLab l1^","^showLab l2 
 | showIR (Lab(l1,instr)) = 
      showLab l1^": "^showIR instr  
 | showIR (JumpI l1) = 
      "jumpI         -> "^showLab l1
 | showIR NOP = "nop"
 
(* ******************************************************* *)

val emitted = ref ([]: IR list);
fun emit x = emitted := (x :: (!emitted));
fun emitAt l x = emit(Lab(l,x));

fun tag l emitter = 
 let val temp = ! emitted
     val _ = emitted := []
     val _ = emitter()
     val new = ! emitted
     fun help [x] old = (Lab(l,x))::old
       | help []  (z :: zs) = (Lab(l,z))::zs
       | help (x::xs) old = x :: help xs old
 in emitted := help new temp end;

val Rarp = 0;
val firstRegister = 1;
val regCount = ref firstRegister;
fun resetRegister () = regCount := firstRegister;
fun NextRegister() = 
  let val n = !regCount 
  in (regCount := n+1; n) end;
  

val firstLabel = 1;
val LabelCount = ref firstLabel;
fun resetLabel () = LabelCount := firstLabel;
fun NextLabel m = 
  let val n = !LabelCount
      fun f n 0 = []
        | f n m = n :: (f (n+1) (m-1))
  in (LabelCount := n+m; f n m) end;  

val firstCC = 1;
val CCCount = ref firstCC;
fun resetCC () = CCCount := firstCC;
fun NextCC() = 
  let val n = !CCCount 
  in (CCCount := n+1; n) end;  



fun offset (Var(loc,x)) = 
  let val result = NextRegister()
  in emit (LoadI("@"^x,result)); 
     result 
  end;
  
fun base x = Rarp;

(* ************************************************* *)
(* *** examples to test on ************************* *)

fun var x = (Var((0,0),x));
fun const n = Literal((0,0),Cint (Int.toString n));

val ex1 = Binop(SUB,var "x",Binop(MUL,const 2,var "y"));

val ex2 = Binop(SUB,var "x",Binop(MUL,const 2,var "x"));

val ex3 = Binop(OR,var "b",Binop(AND,var "c",Not(var "d")));

val ex4 = Relop(LT,var "x",var "y");

val ex5 = Binop(OR,Relop(LT,var "a",var "b")
                  ,Binop(AND,Relop(LT,var "c",var "d"),
                             Relop(LT,var "e",var "f")));

(* ***************** Variable dictionaries *************** *)

datatype EncodingStyle = Numerical | CondCode | Positional;
val style = ref Positional;

fun flip () =
    (case !style of
       Positional => style := Numerical
     | Numerical => style := CondCode
     | CondCode => style := Positional; !style);


fun compare oper rx ry trueL falseL = 
  let val cc = NextCC()
  in emit (Comp(rx,ry,cc));
     emit (Cbr(oper,cc,trueL,falseL))
  end;

fun fillRelSlot trueL truef falseL falsef =
 let val [resultL] = NextLabel 1
 in tag trueL truef;
    emit (JumpI resultL);
    tag falseL falsef;
    emit (JumpI resultL);
    emitAt resultL Nop
 end;

fun expr dict node =
case node of
  Var(loc,x) => 
     (case List.find (fn (nm,r) => x=nm) (!dict) of
        NONE => let val t1 = base node
                    val t2 = offset node
                    val result = NextRegister()
                in emit (LoadAO(t1,t2,result));
                   dict := (x,result)::(!dict);
                   result 
                end
     | SOME(x,r) => r)
| Binop(AND,_,_) => shortCircuit dict node
| Binop(OR,_,_) => shortCircuit dict node
| Binop(m,x,y) =>
     let val t1 = expr dict x
         val t2 = expr dict y
         val result = NextRegister()
     in emit (Arith(m,t1,t2,result));
        result 
     end
| Literal(loc,Cint n) =>   
     let val result = NextRegister()
     in emit (LoadI(n,result)); 
        result 
     end
| Relop(m,x,y) =>
    let val rx = expr dict x
        val ry = expr dict y
        val r2 = NextRegister()
    in (case !style of
          Numerical => emit (Cmp(m,rx,ry,r2))
        | CondCode =>
            let val cc = NextCC()
                val [l1,l2,l3] = NextLabel 3
            in emit      (Comp(rx,ry,cc));
               emit      (Cbr(m,cc,l1,l2));
               emitAt l1 (LoadI("true",r2));
               emit      (JumpI l3);
               emitAt l2 (LoadI("false",r2));
               emit      (JumpI l3);
               emitAt l3 Nop
            end
        | Positional =>
           let val [trueL,falseL] = NextLabel 2
           in compare m rx ry trueL falseL;
              fillRelSlot trueL  (fn () => emit (LoadI("true",r2)))
                          falseL (fn () => emit (LoadI("false",r2)))
           end
            
            );
       r2
    end
| Not x => 
     let val t1 = expr dict x
         val result = NextRegister()
     in emit (Neg(t1,result));
        result 
     end     


and short dict (Relop(m,x,y)) start trueL falseL = 
      let val _ = emitAt start Nop
          val rx = expr dict x
          val ry = expr dict y
          val cc = NextCC()
      in emit (Comp(rx,ry,cc));
         emit (Cbr(m,cc,trueL,falseL))
      end
  | short dict (Binop(AND,r1,r2)) start trueL falseL = 
      let val [start2] = NextLabel 1
      in short dict r1 start start2 falseL;
         short dict r2 start2 trueL falseL
      end
  | short dict (Binop(OR,r1,r2)) start trueL falseL = 
      let val [start2] = NextLabel 1
      in short dict r1 start trueL start2;
         short dict r2 start2 trueL falseL
      end

and shortCircuit dict exp =
  let val [start,l1,l2,l3] = NextLabel 4
      val r2 = NextRegister()
  in short dict exp start l1 l2;
     emitAt l1 (LoadI("true",r2));
     emit      (JumpI l3);
     emitAt l2 (LoadI("false",r2));
     emit      (JumpI l3);
     emitAt l3 Nop;
     r2
  end;
 


(*
fun positional (trueL,falseL,resultL) (truef,falsef) = 
 ( emit (Lab(l1,LoadI("true",r2)));
               emit (JumpI l3);
               emit (Lab(l2,LoadI("false",r2)));
               emit (JumpI l3);
               emit (Lab(l3,Nop))
*)


fun trans x =
 let val _ = emitted := []
     val _ = resetRegister () 
     val _ = resetCC()
     val _ = resetLabel()
     val dict = ref []
     val result = expr dict x
     val instrs = rev (!emitted)
     val _ = print "\n";
     fun sh x = print(showIR x^"\n")
     val _ = map sh instrs
     val _ = print "\n\n";
 in instrs end;


end