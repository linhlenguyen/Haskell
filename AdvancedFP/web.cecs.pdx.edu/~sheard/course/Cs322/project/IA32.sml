structure IA32 = struct


val wdsize = 4;
fun label32 n =  "L" ^ Int.toString n

exception ia32Err of string
fun err s = (print(s^"\n"); raise (ia32Err s));


datatype Register 
 = eax   (* Accumulator         *)
 | ebx   (* Base                *)
 | ecx   (* Count               *)
 | edx   (* Data                *)
 | esi   (* Source index        *)
 | edi   (* Destination index   *)
 | ebp   (* Base pointer        *)
 | esp   (* Stack Pointer       *)
 | eip   (* Instruction pointer *)
 | eflag (* Flags               *)


type Label = string;

datatype Mode 
 = Mem of string
 | % of Register
 | $ of int
 | Lit of string
 | & of (int * Mode);


(* Every instruction contains a label and a comment *)
(* by using the lower case constructors, both the label *)
(* and the comment are made the empty string *)

datatype IA32 
 = Movl  of (Label * Mode * Mode * string)
 | Xchgl of (Label * Mode * Mode * string)
 | Addl  of (Label * Mode * Mode * string)
 | Subl  of (Label * Mode * Mode * string)
 | Imull of (Label * Mode * Mode * string)
 | Andl  of (Label * Mode * Mode * string)
 | Orl   of (Label * Mode * Mode * string)
 | Xorl  of (Label * Mode * Mode * string)
 | Cmpl  of (Label * Mode * Mode * string)
 | Idivl of (Label * Mode * string)
 | Negl  of (Label * Mode * string)
 | Notl  of (Label * Mode * string)
 | Incl  of (Label * Mode * string)
 | Decl  of (Label * Mode * string)
 | Pushl of (Label * Mode * string)
 | Popl  of (Label * Mode * string)
 | Jmp   of (Label * Label * string)
 | Jz    of (Label * Label * string)
 | Jnz   of (Label * Label * string)
 | Je   of (Label * Label * string)
 | Jne   of (Label * Label * string) 
 | Jl    of (Label * Label * string)
 | Jnl   of (Label * Label * string)
 | Jle   of (Label * Label * string)
 | Jg    of (Label * Label * string)
 | Jng   of (Label * Label * string) 
 | Jge   of (Label * Label * string) 
 | Call  of (Label * Label * string) 
 | Ret of (Label*string)
 | Text of (Label*string)
 | Globl of (Label*Label*string)
 | Data of (Label*string)
 | Asciz of (Label*string*string)
 | Long of (Label*string*string)
 
 
 
(***************** Lower case constructors ******************* *)

fun movl (m1,m2) = Movl("",m1,m2,"");
fun xchgl(m1,m2) = Xchgl("",m1,m2,"");
fun addl (m1,m2) = Addl("",m1,m2,"");
fun subl (m1,m2) = Subl("",m1,m2,"");
fun imull(m1,m2) = Imull("",m1,m2,"");
fun andl (m1,m2) = Andl("",m1,m2,"");
fun orl  (m1,m2) = Orl("",m1,m2,"");
fun xorl (m1,m2) = Xorl("",m1,m2,"");
fun cmpl (m1,m2) = Cmpl("",m1,m2,"");
fun idivl(m1) = Idivl("",m1,"");
fun negl (m1) = Negl("",m1,"");
fun notl (m1) = Notl("",m1,"");
fun incl (m1) = Incl("",m1,"");
fun decl (m1) = Decl("",m1,"");
fun pushl(m1) = Pushl("",m1,"");
fun popl (m1) = Popl("",m1,"");
fun jmp  (l2) = Jmp("",l2,"");
fun jz   (l2) = Jz("",l2,"");
fun jnz  (l2) = Jnz("",l2,"");
fun je   (l2) = Je("",l2,"");
fun jne  (l2) = Jne("",l2,"");
fun jl   (l2) = Jl("",l2,"");
fun jnl  (l2) = Jnl("",l2,"");
fun jle  (l2) = Jle("",l2,"");
fun jg   (l2) = Jg("",l2,"");
fun jng  (l2) = Jng("",l2,"");
fun jge  (l2) = Jge("",l2,"");
fun call  (l2) = Call("",l2,"");
fun return () = Ret("","");

fun text () = Text("","");
fun globl (l2) = Globl("",l2,"");
fun dataX () = Data("","");
fun long s = Long("",s,"");
fun asciz s = Asciz("",s,"");


(* *************** Adding Labels and Comments ************** *)

fun addLabel l x =
case x of
   Movl (_,m1,m2,s) => Movl (l,m1,m2,s)
 | Xchgl(_,m1,m2,s) => Xchgl(l,m1,m2,s)
 | Addl (_,m1,m2,s) => Addl (l,m1,m2,s)
 | Subl (_,m1,m2,s) => Subl (l,m1,m2,s)
 | Imull(_,m1,m2,s) => Imull(l,m1,m2,s)
 | Andl (_,m1,m2,s) => Andl (l,m1,m2,s)
 | Orl  (_,m1,m2,s) => Orl  (l,m1,m2,s)
 | Xorl (_,m1,m2,s) => Xorl (l,m1,m2,s)
 | Cmpl (_,m1,m2,s) => Cmpl (l,m1,m2,s)
 | Idivl(_,m1,s) => Idivl(l,m1,s)
 | Negl (_,m1,s) => Negl (l,m1,s)
 | Notl (_,m1,s) => Notl (l,m1,s)
 | Incl (_,m1,s) => Incl (l,m1,s)
 | Decl (_,m1,s) => Decl (l,m1,s)
 | Pushl(_,m1,s) => Pushl(l,m1,s)
 | Popl (_,m1,s) => Popl (l,m1,s) 
 | Jmp  (_,l2,s) => Jmp  (l,l2,s)
 | Jz   (_,l2,s) => Jz   (l,l2,s)
 | Jnz  (_,l2,s) => Jnz  (l,l2,s)
 | Je   (_,l2,s) => Je   (l,l2,s)
 | Jne  (_,l2,s) => Jne  (l,l2,s) 
 | Jge  (_,l2,s) => Jge  (l,l2,s)
 | Jle  (_,l2,s) => Jle  (l,l2,s)
 | Jl   (_,l2,s) => Jl   (l,l2,s)
 | Jnl  (_,l2,s) => Jnl  (l,l2,s)
 | Jg   (_,l2,s) => Jg   (l,l2,s)
 | Jng  (_,l2,s) => Jng  (l,l2,s)
 | Call (_,l2,s) => Call (l,l2,s)
 | Ret (_,s) => Ret (l,s)
 | Text(_,s) => Text(l,s)
 | Globl(_,l2,s) => Globl(l,l2,s)
 | Data(_,s) => Data(l,s)
 | Asciz(_,l2,s) => Asciz(l,l2,s) 
 | Long(_,s1,s) => Long(l,s1,s)
 


fun addComment s x =
case x of
   Movl (l,m1,m2,_) => Movl (l,m1,m2,s)
 | Xchgl(l,m1,m2,_) => Xchgl(l,m1,m2,s)
 | Addl (l,m1,m2,_) => Addl (l,m1,m2,s)
 | Subl (l,m1,m2,_) => Subl (l,m1,m2,s)
 | Imull(l,m1,m2,_) => Imull(l,m1,m2,s)
 | Andl (l,m1,m2,_) => Andl (l,m1,m2,s)
 | Orl  (l,m1,m2,_) => Orl  (l,m1,m2,s)
 | Xorl (l,m1,m2,_) => Xorl (l,m1,m2,s)
 | Cmpl (l,m1,m2,_) => Cmpl (l,m1,m2,s)
 | Idivl(l,m1,_) => Idivl(l,m1,s)
 | Negl (l,m1,_) => Negl (l,m1,s)
 | Notl (l,m1,_) => Notl (l,m1,s)
 | Incl (l,m1,_) => Incl (l,m1,s)
 | Decl (l,m1,_) => Decl (l,m1,s)
 | Pushl(l,m1,_) => Pushl(l,m1,s)
 | Popl (l,m1,_) => Popl (l,m1,s) 
 | Jmp  (l,l2,_) => Jmp  (l,l2,s)
 | Jz   (l,l2,_) => Jz   (l,l2,s)
 | Jnz  (l,l2,_) => Jnz  (l,l2,s)
 | Je   (l,l2,_) => Je   (l,l2,s)
 | Jne  (l,l2,_) => Jne  (l,l2,s) 
 | Jge  (l,l2,_) => Jge  (l,l2,s)
 | Jle  (l,l2,_) => Jle  (l,l2,s)
 | Jl   (l,l2,_) => Jl   (l,l2,s)
 | Jnl  (l,l2,_) => Jnl  (l,l2,s)
 | Jg   (l,l2,_) => Jg   (l,l2,s)
 | Jng  (l,l2,_) => Jng  (l,l2,s)
 | Call (l,l2,_) => Call (l,l2,s) 
 | Ret (l,_) => Ret(l,s)
 | Text(l,_) => Text(l,s)
 | Globl(l,l2,_) => Globl(l,l2,s)
 | Data(l,_) => Data(l,s)
 | Asciz(l,l2,_) => Asciz(l,l2,s) 
 | Long(l,s1,_) => Long(l,s1,s);
 


(* ********************* Showing things ********************* *)

fun showReg eax = "eax"
  | showReg ebx = "ebx"
  | showReg ecx = "ecx"
  | showReg edx = "edx"
  | showReg esi = "esi"
  | showReg edi = "edi"
  | showReg esp = "esp"
  | showReg ebp = "ebp"
  | showReg eip = "eip"
  | showReg eflag = "eflag"  

fun showi n =
    if n <0 then "-"^showi(~ n)
            else Int.toString n;
      
fun showMode (Mem s) = s
  | showMode (% r) = "%"^showReg r
  | showMode ($ n) = "$"^(showi n)
  | showMode (Lit s) = "$"^s
  | showMode (&(n,m)) = (showi  n)^"("^showMode m^")";

fun body x =
case x of
   Movl (l,m1,m2,s) => "movl" ^"\t"^showMode m1^","^showMode m2
 | Xchgl(l,m1,m2,s) => "xchgl"^"\t"^showMode m1^","^showMode m2
 | Addl (l,m1,m2,s) => "addl" ^"\t"^showMode m1^","^showMode m2
 | Subl (l,m1,m2,s) => "subl" ^"\t"^showMode m1^","^showMode m2
 | Imull(l,m1,m2,s) => "imull"^"\t"^showMode m1^","^showMode m2
 | Andl (l,m1,m2,s) => "andl" ^"\t"^showMode m1^","^showMode m2
 | Orl  (l,m1,m2,s) => "orl"  ^"\t"^showMode m1^","^showMode m2
 | Xorl (l,m1,m2,s) => "xorl" ^"\t"^showMode m1^","^showMode m2
 | Cmpl (l,m1,m2,s) => "cmpl" ^"\t"^showMode m1^","^showMode m2
 | Idivl(l,m1,s) =>    "idivl"^"\t"^showMode m1
 | Negl (l,m1,s) =>    "negl" ^"\t"^showMode m1
 | Notl (l,m1,s) =>    "notl" ^"\t"^showMode m1
 | Incl (l,m1,s) =>    "incl" ^"\t"^showMode m1
 | Decl (l,m1,s) =>    "decl" ^"\t"^showMode m1
 | Pushl(l,m1,s) =>    "pushl"^"\t"^showMode m1
 | Popl (l,m1,s) =>    "popl" ^"\t"^showMode m1
 | Jmp  (l,l2,s) =>    "jmp"  ^"\t"^l2
 | Jz   (l,l2,s) =>    "jz"   ^"\t"^l2
 | Jnz  (l,l2,s) =>    "jnz"  ^"\t"^l2
 | Jl   (l,l2,s) =>    "jl"   ^"\t"^l2
 | Jnl  (l,l2,s) =>    "jnl"  ^"\t"^l2
 | Jg   (l,l2,s) =>    "jg"   ^"\t"^l2
 | Jng  (l,l2,s) =>    "jng"  ^"\t"^l2 
 | Je   (l,l2,s) =>    "je"  ^"\t"^l2 
 | Jne  (l,l2,s) =>    "jne"  ^"\t"^l2 
 | Jge  (l,l2,s) =>    "jge"  ^"\t"^l2 
 | Jle  (l,l2,s) =>    "jle"  ^"\t"^l2 
 
 | Call (l,l2,s) =>    "call" ^"\t"^l2
 | Ret (l,s) =>    "ret" ^"\t"
 | Text(l1,s) => ".text"^"\t"
 | Globl(l1,l2,s) => ".globl"^"\t"^l2^"\n"
 | Data(l1,s) =>  ".data"^"\t"^s
 | Asciz(l1,l2,s) => ".asciz"^"\t\""^l2^"\""
 | Long(l1,s1,s) => ".long"^"\t"^s1


  
(* *********************************************************** *)  

fun label x =
case x of
   Movl (l,m1,m2,s) => l
 | Xchgl(l,m1,m2,s) => l
 | Addl (l,m1,m2,s) => l
 | Subl (l,m1,m2,s) => l
 | Imull(l,m1,m2,s) => l
 | Andl (l,m1,m2,s) => l
 | Orl  (l,m1,m2,s) => l
 | Xorl (l,m1,m2,s) => l
 | Cmpl (l,m1,m2,s) => l
 | Idivl(l,m1,s) => l
 | Negl (l,m1,s) => l
 | Notl (l,m1,s) => l
 | Incl (l,m1,s) => l
 | Decl (l,m1,s) => l
 | Pushl(l,m1,s) => l
 | Popl (l,m1,s) => l
 | Jmp  (l,l2,s) => l
 | Jz   (l,l2,s) => l
 | Jnz  (l,l2,s) => l
 | Jl   (l,l2,s) => l
 | Jnl  (l,l2,s) => l
 | Jg   (l,l2,s) => l
 | Jng  (l,l2,s) => l 
 | Je   (l,l2,s) => l
 | Jne  (l,l2,s) => l
 | Jge  (l,l2,s) => l
 | Jle  (l,l2,s) => l
 | Call (l,l2,s) => l
 | Ret(l,_) => l
 | Text(l,s) => l
 | Globl(l,l2,s) => l 
 | Data(l,s) => l
 | Asciz(l,l2,s) => l  
 | Long(l,s1,s) => l;



fun comment x =
case x of
   Movl (l,m1,m2,s) => s
 | Xchgl(l,m1,m2,s) => s
 | Addl (l,m1,m2,s) => s
 | Subl (l,m1,m2,s) => s
 | Imull(l,m1,m2,s) => s
 | Andl (l,m1,m2,s) => s
 | Orl  (l,m1,m2,s) => s
 | Xorl (l,m1,m2,s) => s
 | Cmpl (l,m1,m2,s) => s
 | Idivl(l,m1,s) => s
 | Negl (l,m1,s) => s
 | Notl (l,m1,s) => s
 | Incl (l,m1,s) => s
 | Decl (l,m1,s) => s
 | Pushl(l,m1,s) => s
 | Popl (l,m1,s) => s
 | Jmp  (l,l2,s) => s
 | Jz   (l,l2,s) => s
 | Jnz  (l,l2,s) => s
 | Je   (l,l2,s) => s
 | Jne  (l,l2,s) => s
 | Jge  (l,l2,s) => s
 | Jle  (l,l2,s) => s
 | Jl   (l,l2,s) => s
 | Jnl  (l,l2,s) => s
 | Jg   (l,l2,s) => s
 | Jng  (l,l2,s) => s 
 | Call (l,l2,s) => s
 | Ret(_,s) => s
 | Text(l,s) => s
 | Globl(l,l2,s) => s 
 | Data(l,s) => s
 | Asciz(l,l2,s) => s
 | Long(l,s1,s) => s;


fun output (stream,s) = TextIO.output(stream,s);

fun show32 stream x = 
 let fun lab x = 
       let val l = label x
       in if l="" then "" else l^":" end
     fun com x = 
       let val l = comment x
       in if l="" then "" else "# "^l end       
in output (stream,lab x^"\t");
   output (stream,body x^"\t");
   output (stream,com x^"\n")
end;   

fun showIa32 xs = app (show32 TextIO.stdOut) xs;    


fun printIa32 file xs = 
  let val stream = TextIO.openOut file
  in app (show32 stream) xs
   ; TextIO.closeOut(stream)
   ; print ("Printed object file to "^file)
  end;
  
end