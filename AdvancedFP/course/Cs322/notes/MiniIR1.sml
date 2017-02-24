structure IR1 = struct



(* This IR is the first in a sequence of IR's *)

datatype BINOP = ADD | SUB | MUL | DIV | AND | OR
datatype RELOP = EQ | NE | LT | LE | GT | GE

datatype EXP
= BINOP of BINOP * EXP * EXP
| CALL of EXP * EXP list
| MEM of EXP
| NAME of string
| TEMP of int
| PARAM of int
| VAR of int
| CONST of string
| STRING of string
| ESEQ of STMT * EXP
| EXPLIST of EXP list

and STMT
= MOVE of EXP * EXP
| JUMP of EXP
| CJUMP of RELOP * EXP * EXP * EXP
| LABEL of EXP
| CALLST of EXP * EXP list
| RETURN of EXP
| STMTlist of STMT list;

datatype FUNC
= FUNC of string * int * int * STMT list

end
