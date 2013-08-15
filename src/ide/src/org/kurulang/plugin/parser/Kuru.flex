package org.kurulang.plugin.parser;
import com.intellij.lexer.*;
import com.intellij.psi.tree.IElementType;
import static org.kurulang.plugin.parser.KuruTokenTypes.*;

%%

%{
  public _Kuru() {
    this((java.io.Reader)null);
  }
%}

%public
%class _Kuru
%implements FlexLexer
%function advance
%type IElementType
%unicode
%eof{ return;
%eof}

COMMENT="(*" ~"*)"
WHITE_SPACE=[\ \r\n\f]+
alphanum=[A-Za-z'_0-9]*
alphanumId=[A-Za-z]{alphanum}
sym=[-!%&$+/:<=>?@~`\^|#*]|"\\"
symId={sym}+
id={alphanumId}|{symId}
longid={id}("."{id})*
num=[0-9]+
frac="."{num}
exponent=[eE](~\?){num}
real=(~\?)(({num}{frac}?{exp})|({num}{frac}{exponent}?))
hexDigit=[0-9a-fA-F]
hexnum={hexDigit}+

%state S
%state C

%%
<YYINITIAL>{WHITE_SPACE}        { return com.intellij.psi.TokenType.WHITE_SPACE; }
<YYINITIAL>{COMMENT}       { return COMMENT; }
<YYINITIAL>"_"     { return WILD; }
<YYINITIAL>","     { return COMMA; }
<YYINITIAL>"{"     { return LBRACE; }
<YYINITIAL>"}"     { return RBRACE; }
<YYINITIAL>"["     { return LBRACKET; }
<YYINITIAL>"]"     { return RBRACKET; }
<YYINITIAL>";"     { return SEMICOLON; }
<YYINITIAL>"("     { return LPAREN; }
<YYINITIAL>")"     { return RPAREN; }
<YYINITIAL>"..."   { return DOTDOTDOT; }
<YYINITIAL>"|"  { return BAR; }
<YYINITIAL>":"  { return COLON; }
<YYINITIAL>":>"  { return COLONGT; }
<YYINITIAL>"="  { return EQUALOP; }
<YYINITIAL>"#"  { return HASH; }
<YYINITIAL>"->"  { return ARROW; }
<YYINITIAL>"=>"  { return DARROW; }
<YYINITIAL>"and"  { return AND; }
<YYINITIAL>"abstype"  { return ABSTYPE; }
<YYINITIAL>"as"  { return AS; }
<YYINITIAL>"case"  { return CASE; }
<YYINITIAL>"datatype"  { return DATATYPE; }
<YYINITIAL>"else"  { return ELSE; }
<YYINITIAL>"end"  { return END; }
<YYINITIAL>"eqtype"  { return EQTYPE; }
<YYINITIAL>"exception"  { return EXCEPTION; }
<YYINITIAL>"do"  { return DO; }
<YYINITIAL>"fn"  { return FN; }
<YYINITIAL>"fun"  { return FUN; }
<YYINITIAL>"functor"  { return FUNCTOR; }
<YYINITIAL>"handle"  { return HANDLE; }
<YYINITIAL>"if"  { return IF; }
<YYINITIAL>"in"  { return IN; }
<YYINITIAL>"include"  { return INCLUDE; }
<YYINITIAL>"infix"  { return INFIX; }
<YYINITIAL>"infixr"  { return INFIXR; }
<YYINITIAL>"let"  { return LET; }
<YYINITIAL>"local"  { return LOCAL; }
<YYINITIAL>"nonfix"  { return NONFIX; }
<YYINITIAL>"of"  { return OF; }
<YYINITIAL>"op"  { return OP; }
<YYINITIAL>"open"  { return OPEN; }
<YYINITIAL>"raise"  { return RAISE; }
<YYINITIAL>"rec"  { return REC; }
<YYINITIAL>"sharing"  { return SHARING; }
<YYINITIAL>"sig"  { return SIG; }
<YYINITIAL>"signature"  { return SIGNATURE; }
<YYINITIAL>"struct"  { return STRUCT; }
<YYINITIAL>"structure"  { return STRUCTURE; }
<YYINITIAL>"then"  { return THEN; }
<YYINITIAL>"type"  { return TYPE; }
<YYINITIAL>"val"  { return VAL; }
<YYINITIAL>"where"  { return WHERE; }
<YYINITIAL>"while"  { return WHILE; }
<YYINITIAL>"with"  { return WITH; }
<YYINITIAL>"withtype"  { return WITHTYPE; }
<YYINITIAL>"orelse"  { return ORELSE; }
<YYINITIAL>"andalso"  { return ANDALSO; }
<YYINITIAL>"'"{alphanum} => { return TYVAR; }
<YYINITIAL>{longid} => { return LONGID; }
<YYINITIAL>"\"" => { yybegin(S); }

<S>"\\\"" => {}
<S>"\"" => { yybegin(YYINITIAL); return STRING; }
<S>. => {}

[^] { return com.intellij.psi.TokenType.BAD_CHARACTER; }
