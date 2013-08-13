package org.kurulang.plugin;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import org.kurulang.plugin.psi.KuruTypes;
import com.intellij.psi.TokenType;

%%

%class KuruLexer
%implements FlexLexer
%unicode
%function advance
%type IElementType
%eof{  return;
%eof}

WHITE_SPACE=[\ \t\f\n\r]
alphanum=[A-Za-z'_0-9]*
alphanumId=[A-Za-z]{alphanum}
sym=("-"|"!"|"%"|"&"|"$"|"+"|"/"|":"|"<"|"="|">"|"?"|"@"|"~"|"`"|"^"|"#"|"*"|"\\")
symId={sym}+
IDENT={alphanumId}|{symId}
LONGID={IDENT}("."{IDENT})*
TYVAR="'"{IDENT}
NUM=[~]?(0|([1-9][0-9]*))
REAL={NUM}"."[0-9]+
COMMENT="(*" ~"*)"

%state STRING

%%

<YYINITIAL> "val"                                           { return KuruTypes.VALDEC; }
<YYINITIAL> "fun"                                           { return KuruTypes.FUNDEC; }
<YYINITIAL> "datatype"                                      { return KuruTypes.DTDEC; }
<YYINITIAL> "type"                                          { return KuruTypes.TYPEDEC; }
<YYINITIAL> "structure"                                     { return KuruTypes.STRUCTDEC; }
<YYINITIAL> "signature"                                     { return KuruTypes.SIGDEC; }
<YYINITIAL> "struct"                                        { return KuruTypes.STRUCT; }
<YYINITIAL> "sig"                                           { return KuruTypes.SIG; }
<YYINITIAL> "rec"                                           { return KuruTypes.REC; }
<YYINITIAL> "of"                                            { return KuruTypes.OF; }
<YYINITIAL> "as"                                            { return KuruTypes.AS; }
<YYINITIAL> "and"                                           { return KuruTypes.AND; }
<YYINITIAL> "withtype"                                      { return KuruTypes.WITHTYPE; }
<YYINITIAL> "andalso"                                       { return KuruTypes.ANDALSO; }
<YYINITIAL> "orelse"                                        { return KuruTypes.ORELSE; }
<YYINITIAL> "fn"                                            { return KuruTypes.FN; }
<YYINITIAL> "do"                                            { return KuruTypes.DO; }
<YYINITIAL> "if"                                            { return KuruTypes.IF; }
<YYINITIAL> "then"                                          { return KuruTypes.THEN; }
<YYINITIAL> "else"                                          { return KuruTypes.ELSE; }
<YYINITIAL> "while"                                         { return KuruTypes.WHILE; }
<YYINITIAL> "let"                                           { return KuruTypes.LET; }
<YYINITIAL> "in"                                            { return KuruTypes.IN; }
<YYINITIAL> "end"                                           { return KuruTypes.END; }
<YYINITIAL> "eqtype"                                        { return KuruTypes.TYPEDEC; }
<YYINITIAL> "case"                                          { return KuruTypes.CASE; }
<YYINITIAL> "handle"                                        { return KuruTypes.HANDLE; }
<YYINITIAL> "raise"                                         { return KuruTypes.RAISE; }
<YYINITIAL> "infix"                                         { return KuruTypes.INFIX; }
<YYINITIAL> "infixr"                                        { return KuruTypes.INFIX; }
<YYINITIAL> "open"                                          { return KuruTypes.OPEN; }
<YYINITIAL> "local"                                         { return KuruTypes.LOCAL; }
<YYINITIAL> "_import"                                       { return KuruTypes.IMPORT; }
<YYINITIAL> "true"                                          { return KuruTypes.LITERAL; }
<YYINITIAL> "false"                                         { return KuruTypes.LITERAL; }

<YYINITIAL> "="                                             { return KuruTypes.EQ; }
<YYINITIAL> "_"                                             { return KuruTypes.WILD; }
<YYINITIAL> "["                                             { return KuruTypes.LSQ; }
<YYINITIAL> "]"                                             { return KuruTypes.RSQ; }
<YYINITIAL> "::"                                            { return KuruTypes.CONS; }
<YYINITIAL> ":"                                             { return KuruTypes.COLON; }
<YYINITIAL> ";"                                             { return KuruTypes.SEMI; }
<YYINITIAL> "|"                                             { return KuruTypes.BAR; }

<YYINITIAL> "->"                                            { return KuruTypes.TARROW; }
<YYINITIAL> "("                                             { return KuruTypes.LPAR; }
<YYINITIAL> ")"                                             { return KuruTypes.RPAR; }
<YYINITIAL> ","                                             { return KuruTypes.COMMA; }
<YYINITIAL>{TYVAR}                                          { return KuruTypes.TYPEVAR; }
<YYINITIAL>{LONGID}                                         { return KuruTypes.IDENTIFIER; }
<YYINITIAL>{REAL}                                           { return KuruTypes.LITERAL; }
<YYINITIAL>{NUM}                                            { return KuruTypes.LITERAL; }

<YYINITIAL>"\""                                             { yybegin(STRING); }

<STRING> {
  \"                             { yybegin(YYINITIAL); return KuruTypes.STRING; }
  [^\n\r\"\\]+                   { ; }
  \\t                            { ; }
  \\n                            { ; }

  \\r                            { ; }
  \\\"                           { ; }
  \\                             { ; }
}

{COMMENT}                          { yybegin(YYINITIAL); return KuruTypes.COMMENT; }
{WHITE_SPACE}+                     { yybegin(YYINITIAL); return TokenType.WHITE_SPACE; }
 
.                                  { return TokenType.BAD_CHARACTER; }