type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token
val lineno = ref 1;
val charno = ref 0;

open ProgramTypes
open Tokens

type integer = IntInf.int;
val maxInt = 2147483647:integer;

fun initLexer () = (lineno := 1; charno := 0);
fun posn () = (!lineno, !charno);
fun eof () = EOF(!lineno,!lineno);
%%
%header (functor MiniLexFun(structure Tokens: Mini_TOKENS));
%%

\n => ( lineno := !lineno + 1; charno := 0; lex() );
\"[\032\033\035-\126]*\" => ( (* string literal - note \034 = quote mark *)
  let val len = String.size yytext
   in if len <= 257 
         then ( charno := !charno + len;
                String (String.substring (yytext, 1, len - 2),!lineno,!lineno))
         else raise LexicalError ("String literal too long ("
                                  ^ Int.toString len ^ "):\n\""
                                  ^ String.substring (yytext, 1, 255)
                                  ^ "(...)\"", posn())
  end);
[\t\ \013]+     => ( charno := !charno + (String.size yytext); lex() );
"//".*\n        => ( lineno := !lineno + 1; charno := 0; lex() );
"/*"\/{0,1}([^/]|[^*]\/)*"*/" => (
  let val comlines = String.tokens (fn x => x = #"\n") yytext
      in charno := String.size (List.last comlines);
         lineno := !lineno + length comlines - 1;
         lex()
      end);

boolean              => ( charno := !charno + 7; KW_boolean(!lineno,!charno) );
class                => ( charno := !charno + 5; KW_class(!lineno,!charno) );
double               => ( charno := !charno + 6; KW_double(!lineno,!charno) );
else                 => ( charno := !charno + 4; KW_else(!lineno,!charno) );
extends              => ( charno := !charno + 7; KW_extends(!lineno,!charno) );
false                => ( charno := !charno + 5; KW_false(!lineno,!charno) );
if                   => ( charno := !charno + 2; KW_if(!lineno,!charno) );
int                  => ( charno := !charno + 3; KW_int(!lineno,!charno) );
length               => ( charno := !charno + 6; KW_length(!lineno,!charno) );
main                 => ( charno := !charno + 4; KW_main(!lineno,!charno) );
new                  => ( charno := !charno + 3; KW_new(!lineno,!charno) );
public               => ( charno := !charno + 6; KW_public(!lineno,!charno) );
return               => ( charno := !charno + 6; KW_return(!lineno,!charno) );
static               => ( charno := !charno + 6; KW_static(!lineno,!charno) );
String               => ( charno := !charno + 6; KW_String(!lineno,!charno) );
System\.out\.println => ( charno := !charno + 18; KW_println(!lineno,!charno) );
this                 => ( charno := !charno + 4; KW_this(!lineno,!charno) );
true                 => ( charno := !charno + 4; KW_true(!lineno,!charno) );
void                 => ( charno := !charno + 4; KW_void(!lineno,!charno) );
while                => ( charno := !charno + 5; KW_while(!lineno,!charno) );

[a-zA-Z][a-zA-Z0-9]* => (
  let val len = String.size yytext
   in if len <= 255
         then ( charno := !charno + len; Identifier (yytext, !lineno, !charno))
         else raise LexicalError ("Identifier too long ("
                                  ^ Int.toString len ^ "):\n\""
                                  ^ String.substring (yytext, 0, 255)
                                  ^ "(...)\"", posn())
  end);

";" => ( charno := !charno + 1; D_semicolon(!lineno,!charno) );
"," => ( charno := !charno + 1; D_comma(!lineno,!charno)     );
"." => ( charno := !charno + 1; D_dot(!lineno,!charno)       );
"(" => ( charno := !charno + 1; D_lparen(!lineno,!charno)    );
")" => ( charno := !charno + 1; D_rparen(!lineno,!charno)    );
"[" => ( charno := !charno + 1; D_lbracket(!lineno,!charno)  );
"]" => ( charno := !charno + 1; D_rbracket(!lineno,!charno)  );
"{" => ( charno := !charno + 1; D_lbrace(!lineno,!charno)    );
"}" => ( charno := !charno + 1; D_rbrace(!lineno,!charno)    );

"="  => ( charno := !charno + 1; OP_assign(!lineno,!charno) );
"+"  => ( charno := !charno + 1; OP_plus(!lineno,!charno)   );
"-"  => ( charno := !charno + 1; OP_minus(!lineno,!charno)  );
"*"  => ( charno := !charno + 1; OP_times(!lineno,!charno)  );
"/"  => ( charno := !charno + 2; OP_div(!lineno,!charno)    );
"&&" => ( charno := !charno + 2; OP_and(!lineno,!charno)    );
"||" => ( charno := !charno + 2; OP_or(!lineno,!charno)     );
"!"  => ( charno := !charno + 1; OP_not(!lineno,!charno)    );
"==" => ( charno := !charno + 2; OP_eq(!lineno,!charno)     );
"!=" => ( charno := !charno + 2; OP_neq(!lineno,!charno)    );
"<"  => ( charno := !charno + 1; OP_le(!lineno,!charno)     );
"<=" => ( charno := !charno + 2; OP_leq(!lineno,!charno)    );
">"  => ( charno := !charno + 1; OP_ge(!lineno,!charno)     );
">=" => ( charno := !charno + 2; OP_geq(!lineno,!charno)    );

[0-9]+\.[0-9]* => ( (* there don't seem to be defined bounds on reals
                     * in the MINI language... *)
                    charno := !charno + (String.size yytext); 
                    Real (yytext, !lineno, !charno) );

[0-9]+ => ( (* rationale for valOf: since yytext will contain only digits, and
             * we're converting to an arbitrary precision integer type,
             * fromString should never fail. *)
             let val n = (valOf o IntInf.fromString) yytext
              in if n <= maxInt
                 then ( charno := !charno + (String.size yytext);
                        Integer (yytext,!lineno,!charno) )
                 else raise LexicalError ("Integer too large: "
                                          ^ yytext, posn())
             end);

. => ( raise LexicalError ("Bad character: [" ^ yytext ^ "]\n", posn()) );
