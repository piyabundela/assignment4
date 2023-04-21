structure T = Tokens

type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token
type lexarg = string
type arg = lexarg

val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;

val eof = fn filename => (lin := 1; col := 0; T.TOK_EOF (!lin, !col));

fun inc a = a := !a + 1

%%
%header (functor ass4(structure Tokens: While_TOKENS));
%arg (fileName: string);
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
eol = ("\013\010"|"\010"|"\013");

%%
{ws}* => (continue ());
{eol} => (inc lin; eolpos:=yypos+size yytext; continue ());

"rational" => (col:=yypos-(!eolpos); T.TOK_rational(!lin,!col));
"integer" => (col:=yypos-(!eolpos); T.TOK_integer(!lin,!col));
"boolean" => (col:=yypos-(!eolpos); T.TOK_boolean(!lin,!col));
"tt" => (col:=yypos-(!eolpos); T.TOK_tt(!lin,!col));
"ff" => (col:=yypos-(!eolpos); T.TOK_ff(!lin,!col));
"var" => (col:=yypos-(!eolpos); T.TOK_var(!lin,!col));
"if" => (col:=yypos-(!eolpos); T.TOK_if(!lin,!col));
"then" => (col:=yypos-(!eolpos); T.TOK_then(!lin,!col));
"else" => (col:=yypos-(!eolpos); T.TOK_else(!lin,!col));
"fi" => (col:=yypos-(!eolpos); T.TOK_fi(!lin,!col));
"while" => (col:=yypos-(!eolpos); T.TOK_while(!lin,!col));
"do" => (col:=yypos-(!eolpos); T.TOK_do(!lin,!col));
"od" => (col:=yypos-(!eolpos); T.TOK_od(!lin,!col));
"procedure" => (col:=yypos-(!eolpos); T.TOK_procedure(!lin,!col));
"print" => (col:=yypos-(!eolpos); T.TOK_print(!lin,!col));
"read" => (col:=yypos-(!eolpos); T.TOK_read(!lin,!col));
"call" => (col:=yypos-(!eolpos); T.TOK_call(!lin,!col));

"make_rat" => (col:=yypos-(!eolpos); T.TOK_make_rat(!lin,!col));
"rat" => (col:=yypos-(!eolpos); T.TOK_rat(!lin,!col));
"showRat" => (col:=yypos-(!eolpos); T.TOK_showRat(!lin,!col));
"showDecimal" => (col:=yypos-(!eolpos); T.TOK_showDecimal(!lin,!col));
"formDecimal" => (col:=yypos-(!eolpos); T.TOK_formDecimal(!lin,!col));
"toDecimal" => (col:=yypos-(!eolpos); T.TOK_toDecimal(!lin,!col));

"+" => (col:=yypos-(!eolpos); T.TOK_ADD(!lin,!col));
"~" => (col:=yypos-(!eolpos); T.TOK_UMINUS(!lin,!col));
"-" => (col:=yypos-(!eolpos); T.TOK_SUB(!lin,!col));
"*" => (col:=yypos-(!eolpos); T.TOK_MUL(!lin,!col));
"/" => (col:=yypos-(!eolpos); T.TOK_DIV(!lin,!col));
"%" => (col:=yypos-(!eolpos); T.TOK_MOD(!lin,!col));
"&&" => (col:=yypos-(!eolpos); T.TOK_AND(!lin,!col));
"||" => (col:=yypos-(!eolpos); T.TOK_OR(!lin,!col));
"!" => (col:=yypos-(!eolpos); T.TOK_NOT(!lin,!col));
"=" => (col:=yypos-(!eolpos); T.TOK_EQ(!lin,!col));
"<>" => (col:=yypos-(!eolpos); T.TOK_NE(!lin,!col));
">" => (col:=yypos-(!eolpos); T.TOK_GT(!lin,!col));
">=" => (col:=yypos-(!eolpos); T.TOK_GE(!lin,!col));
"<" => (col:=yypos-(!eolpos); T.TOK_LT(!lin,!col));
"<=" => (col:=yypos-(!eolpos); T.TOK_LE(!lin,!col));
":=" => (col:=yypos-(!eolpos); T.TOK_ASSIGN(!lin,!col));

"{" => (col:=yypos-(!eolpos); T.TOK_LBRACE(!lin,!col));
"}" => (col:=yypos-(!eolpos); T.TOK_RBRACE(!lin,!col));
"(" => (col:=yypos-(!eolpos); T.TOK_LPAREN(!lin,!col));
")" => (col:=yypos-(!eolpos); T.TOK_RPAREN(!lin,!col));
"," => (col:=yypos-(!eolpos); T.TOK_COMMA(!lin,!col));
";" => (col:=yypos-(!eolpos); T.TOK_SEMICOLON(!lin,!col));

{digit}+ => (col:=yypos-(!eolpos);
        T.TOK_NUM(foldl (fn(a,r)=>ord(a)-ord(#"0")+10*r) 0 (explode yytext), !lin, !col));
[A-Za-z][A-Za-z0-9]* => (col:=yypos-(!eolpos);T.TOK_ID(yytext,!lin,!col));

. => (print ("Unknown token found at " ^ (Int.toString (!lin)) ^ ": <" ^ yytext ^ ">. Continuing.\n"); continue());
