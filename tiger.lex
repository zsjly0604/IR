type svalue = Tokens.svalue
type pos = int
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val stringStartPos = ref 0
val stringContent = ref ""

fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%% 
  %header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
  %s COMMENT STRING ESCAPE WHITESPACE ILLEGALSTRING;
%%
<INITIAL>type    => (Tokens.TYPE(yypos,yypos+size yytext));
<INITIAL>var  	=> (Tokens.VAR(yypos,yypos+size yytext));
<INITIAL>function => (Tokens.FUNCTION(yypos,yypos+size yytext));
<INITIAL>break   => (Tokens.BREAK(yypos,yypos+size yytext));
<INITIAL>of      => (Tokens.OF(yypos,yypos+size yytext));
<INITIAL>end     => (Tokens.END(yypos,yypos+size yytext));
<INITIAL>in      => (Tokens.IN(yypos,yypos+size yytext));
<INITIAL>nil     => (Tokens.NIL(yypos,yypos+size yytext));
<INITIAL>let     => (Tokens.LET(yypos,yypos+size yytext));
<INITIAL>do      => (Tokens.DO(yypos,yypos+size yytext));
<INITIAL>to      => (Tokens.TO(yypos,yypos+size yytext));
<INITIAL>for     => (Tokens.FOR(yypos,yypos+size yytext));
<INITIAL>while   => (Tokens.WHILE(yypos,yypos+size yytext));
<INITIAL>else    => (Tokens.ELSE(yypos,yypos+size yytext));
<INITIAL>then    => (Tokens.THEN(yypos,yypos+size yytext));
<INITIAL>if      => (Tokens.IF(yypos,yypos+size yytext));
<INITIAL>array   => (Tokens.ARRAY(yypos,yypos+5));

<INITIAL>":="      => (Tokens.ASSIGN(yypos,yypos+size yytext));
<INITIAL>"|"      => (Tokens.OR(yypos,yypos+size yytext));
<INITIAL>"&"     => (Tokens.AND(yypos,yypos+size yytext));
<INITIAL>">="      => (Tokens.GE(yypos,yypos+size yytext));
<INITIAL>">"       => (Tokens.GT(yypos,yypos+size yytext));
<INITIAL>"<="      => (Tokens.LE(yypos,yypos+size yytext));
<INITIAL>"<"       => (Tokens.LT(yypos,yypos+size yytext));
<INITIAL>"<>"      => (Tokens.NEQ(yypos,yypos+size yytext));
<INITIAL>"="       => (Tokens.EQ(yypos,yypos+size yytext));

<INITIAL>"/"     => (Tokens.DIVIDE(yypos,yypos+size yytext));
<INITIAL>"*"     => (Tokens.TIMES(yypos,yypos+size yytext));
<INITIAL>"-"       => (Tokens.MINUS(yypos,yypos+size yytext));
<INITIAL>"+"       => (Tokens.PLUS(yypos,yypos+size yytext));
<INITIAL>"."       => (Tokens.DOT(yypos,yypos+size yytext));

<INITIAL>"}"       => (Tokens.RBRACE(yypos,yypos+size yytext));
<INITIAL>"{"       => (Tokens.LBRACE(yypos,yypos+size yytext));
<INITIAL>")"       => (Tokens.RPAREN(yypos,yypos+size yytext));
<INITIAL>"("       => (Tokens.LPAREN(yypos,yypos+size yytext));
<INITIAL>"]"       => (Tokens.RBRACK(yypos,yypos+size yytext));
<INITIAL>"["       => (Tokens.LBRACK(yypos,yypos+size yytext));   

<INITIAL>";"       => (Tokens.SEMICOLON(yypos,yypos+size yytext));
<INITIAL>","	=> (Tokens.COMMA(yypos,yypos+size yytext));
<INITIAL>":"     => (Tokens.COLON(yypos,yypos+size yytext));

<INITIAL>[a-zA-Z][a-zA-Z0-9_]*  => (Tokens.ID(yytext,yypos,yypos+size yytext));
<INITIAL>[0-9]+  => (Tokens.INT(valOf(Int.fromString yytext),yypos,yypos+size yytext));

<INITIAL> \"     => (YYBEGIN STRING;stringContent:="";stringStartPos:= yypos;continue());	     

<STRING>  \"     => (YYBEGIN INITIAL;Tokens.STRING(!stringContent,!stringStartPos,yypos));
<STRING>  \\      => (YYBEGIN ESCAPE;continue());
<STRING> \n      => (YYBEGIN ILLEGALSTRING;ErrorMsg.error yypos ("illegal newline in string"); continue());
<STRING> .     => (stringContent := !stringContent ^yytext;continue());		

<ESCAPE> [0-1][0-9][0-9]  => (YYBEGIN STRING;stringContent := !stringContent^String.str(Char.chr(valOf(Int.fromString yytext)));continue());
<ESCAPE> [2][0-4][0-9]  => (YYBEGIN STRING;stringContent := !stringContent^String.str(Char.chr(valOf(Int.fromString yytext)));continue());
<ESCAPE> [2][5][0-5]  => (YYBEGIN STRING;stringContent := !stringContent^String.str(Char.chr(valOf(Int.fromString yytext)));continue());			  

<ESCAPE> \\       => (YYBEGIN STRING;stringContent := !stringContent ^ "\\";continue());
<ESCAPE> n      => (YYBEGIN STRING;stringContent := !stringContent ^ "\n";continue());
<ESCAPE> t       => (YYBEGIN STRING;stringContent := !stringContent ^ "\t";continue());
<ESCAPE> \"       => (YYBEGIN STRING;stringContent := !stringContent ^ "\"";continue());
<ESCAPE> [ \t\n\r\f]      => (YYBEGIN WHITESPACE;continue());
<ESCAPE> [0-9]+  => (YYBEGIN ILLEGALSTRING;ErrorMsg.error yypos ("illegal escape ascii character in string:"^"\\"^yytext);continue());
<ESCAPE> .       => (YYBEGIN ILLEGALSTRING;ErrorMsg.error yypos ("illegal escape character in string:"^"\\"^yytext);continue());

<ILLEGALSTRING> \"  => (YYBEGIN INITIAL;continue());
<ILLEGALSTRING> .   => (continue());

<WHITESPACE> [ \t\n\r\f]  => (continue());  
<WHITESPACE> \\  => (YYBEGIN STRING;continue());
<WHITESPACE> .   => (YYBEGIN ILLEGALSTRING;ErrorMsg.error yypos ("illegal whitespace character in string:"^"\\"^yytext^"\\");continue());

<INITIAL>"/*"    => (YYBEGIN COMMENT;continue());
<COMMENT>.       => (continue());
<COMMENT>"*/"    => (YYBEGIN INITIAL;continue());

<INITIAL,COMMENT>[ \t\f]          => (continue());
<INITIAL,COMMENT>[\n\r]	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

