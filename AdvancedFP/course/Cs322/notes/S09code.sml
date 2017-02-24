open IR1;

fun bool2int true = 1
  | bool2int false = 0

datatype VAL
 = VNUM of int
 | VFUN of (int -> VAL list -> VAL)
 | VSTR of string

type env = 
        { mem   : int Array.array
        , temps : int Array.array
        , names : (string * VAL ref) list
        , paramReg: int  
        , varReg: int
        , stackPtr : int
        , goto  : LABEL -> STMT list }

exception EvalError of string

fun fromNum (VNUM i) = i
  | fromNum _ = raise (EvalError "non-number")

fun fromFun (VFUN f) = f
  | fromFun _ = raise (EvalError "non-function")


fun scan (LABEL thislab :: stmts) lab =
       if thislab = lab then stmts else scan stmts lab
  | scan (STMTlist ys :: stmts) lab = scan (ys @ stmts) lab
  | scan (COMMENT(s,mess) :: stmts) lab = scan (s::stmts) lab
  | scan (stmt::stmts) lab = scan stmts lab
  | scan [] lab = raise (EvalError "bad label")

fun newGoto ({mem =a, temps = t, names=b, paramReg=c, varReg = e, stackPtr = x, goto = f}) stmts =
   {mem =a, temps =t, names=b, paramReg =c, varReg = e, stackPtr = x, goto = scan stmts};

fun evalEXP (r:env) e = case e of
  BINOP(bop,e1,e2) => 
    (case (evalEXP r e1,evalEXP r e2) of
       (VNUM n, VNUM m) => VNUM(ProgramTypes.evalBINOP bop n m)
     | _ => raise (EvalError "Non-number in BINOP"))
| RELOP(rop,e1,e2) => 
    (case (evalEXP r e1,evalEXP r e2) of
       (VNUM n, VNUM m) => VNUM(bool2int(ProgramTypes.evalRELOP rop n m))
     | _ => raise (EvalError "   on-number in RELOP"))
| CALL(func,args) => 
    fromFun (evalEXP r func) (#stackPtr r) (map (evalEXP r) args)
| MEM(addr)     => VNUM (Array.sub(#mem r, fromNum(evalEXP r addr)))
| NAME(name)    => 
    (case List.find (fn (x,v) => name=x) (#names r) of
        SOME (_,v) => !v
     | NONE => raise (EvalError ("bad name :" ^ name)))
| TEMP(i)       => VNUM (Array.sub(#temps r, i))
| PARAM(i)      => VNUM (Array.sub(#mem r, #paramReg r + i))
| VAR(i)        => VNUM (Array.sub(#mem r, #varReg r + i))

| MEMBER(obj,i) => VNUM (Array.sub(#mem    r, i + fromNum (evalEXP r obj)))

| CONST(str,ProgramTypes.Int)    => VNUM(valOf(Int.fromString str))
| CONST("0",ProgramTypes.Bool)    => VNUM 0
| CONST("1",ProgramTypes.Bool)    => VNUM 1
| STRING(str)   => VSTR str
| ESEQ(stmts,result) => 
        (evalSTMTs (newGoto r stmts) stmts; evalEXP r result)

and evalSTMTs (r:env) stmts = case stmts of
      [] => raise (EvalError "empty statement list")
    | (stmt::rest) => (case stmt of
          MOVE(dst,src) => 
                (Array.update
                        ( #mem r
                        , fromNum (evalEXP r dst)
                        , fromNum (evalEXP r src) )
                ; evalSTMTs r rest )
        | LABEL(label) =>
                evalSTMTs r rest
        | CALLST(func,args) =>
                ( fromFun (evalEXP r func) (#stackPtr r) (map (evalEXP r) args)
                ; evalSTMTs r rest )
        | STMTlist(stmts) => evalSTMTs r (stmts @ rest)
       
        (* control flow statements might not execute the "rest" *)
        | JUMP(label) => 
                evalSTMTs r (#goto r label)
        | CJUMP(relop,e1,e2,label) => 
                if ProgramTypes.evalRELOP relop
                        (fromNum (evalEXP r e1))
                        (fromNum (evalEXP r e2))
                  then evalSTMTs r (#goto r label)
                  else evalSTMTs r rest
        | RETURN(e) =>
                evalEXP r e
        | COMMENT(x,message) => evalSTMTs r (x::rest)
        )

fun funcName (FUNC(fname,_,_,_)) = fname



fun evalFUNC mem names temps (FUNC(fname,paramTypes,varTypes,body)) sp args =
    let 
        fun goto lab = scan body lab
            val actualps = length args
            val ps = length paramTypes + 1
            val _ = if ps=actualps then ()
                       else raise (EvalError (fname^" has wrong number of arguments."))
            val vs = length varTypes
        val r = { mem = mem
                , names = names
                , temps = temps
                , paramReg = sp
                , varReg = sp + ps
                , stackPtr = sp + ps + vs
                , goto = goto }
     in
        evalSTMTs r body
    end

fun evalPRGM env funcs = 
    let
        val mem   = Array.array(10000,0)
        val temps   = Array.array(500,0)
        val names = map (fn (name,v) => (name,ref v)) env
                  @ map (fn f => (funcName f,ref (VNUM 0))) funcs
        fun patch func = 
                case List.find (fn (f,_) => f = funcName func) names of
                  SOME (_,r) => r := VFUN (evalFUNC mem names temps func)
        val _ = List.app patch funcs
     in case List.find (fn (x,f) => x="main") names of
          SOME(_,f) => fromFun (!f) 0 []
        | NONE => raise (EvalError "No main method")
    end
