structure Canonical = struct

open IR1;
open ProgramTypes;

exception BAD of string;
fun bad s = raise(BAD s);

fun canonicalE x =
case x of
  BINOP(m,a,b) =>
    let val (sa,ea) = canonicalE a
        val (sb,eb) = canonicalE b
    in (sa@sb,BINOP(m,ea,eb)) end
| CALL(f,xs) => 
    let val temp = newTemp()
        val (sf,ef) = canonicalE f
        val (xsStmt,xsL) = canonicalL xs
    in (sf @ xsStmt @ [ MOVE(temp,CALL(ef,xsL)) ],temp) end


and canonicalL [] = ([],[])
  | canonicalL (x :: xs) = 
     let val (xStmt,xL) = canonicalE x
         val (xsStmt,xsL) = canonicalL xs
     in (xStmt @ xsStmt, xL :: xsL) end    

and canonicalS x =
case x of
  MOVE(a,b) => 
  let val (sa,ea) = canonicalE a
      val (sb,eb) = canonicalE b
  in sa @ sb @ [MOVE(ea,eb)] end
| STMTlist xs => List.concat (map canonicalS xs)

(* ***********************************************
fun pass1M className env (MetDecl(loc,rng,name,params,vars,body)) =
  let fun paramTypes (Formal(typ,nm)) = typ
      fun paramEnv count [] = []
        | paramEnv count ((Formal(typ,nm))::xs) = 
             (nm,Vparam count)::(paramEnv (count+1) xs)
      fun varTypes (VarDecl(loc,typ,nm,init)) = typ
      fun varEnv count [] = []
        | varEnv count ((VarDecl(loc,typ,nm,init))::xs) = 
             (nm,Vlocal count)::(varEnv (count+1) xs)
      val initEnv = (paramEnv 1 params) @ env
      fun initIR count [] = []
        | initIR count (VarDecl(loc,typ,nm,SOME init) :: vs) = 
             (MOVE(VAR count,pass1E initEnv init))::
             initIR (count+1) vs
        | initIR count (VarDecl(loc,typ,nm,NONE)::xs) = 
             initIR (count+1) xs
      val varIR = initIR 1 vars
      val bodyEnv = (varEnv 1 vars) @ initEnv
      val bodyIR = pass1S bodyEnv (Block body)
in (FUNC(className^"_"^name
        ,map paramTypes params
        ,map varTypes vars
        ,varIR @ bodyIR))
end

* **************************************************** *)

end