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
BLOCK_COMMENT = "(*" [^*] ~"*)" | "(*" "*"+ ")"

%state STRING

%%

<YYINITIAL> "val"                                           { return KuruTypes.VALDEC; }
<YYINITIAL> "fun"                                           { return KuruTypes.FUNDEC; }
<YYINITIAL> "datatype"                                      { return KuruTypes.DTDEC; }
<YYINITIAL> "type"                                          { return KuruTypes.TYPEDEC; }
<YYINITIAL> "structure"                                     { return KuruTypes.STRUCTDEC; }
<YYINITIAL> "signature"                                     { return KuruTypes.SIGDEC; }

<YYINITIAL>{BLOCK_COMMENT}                                  { yybegin(YYINITIAL); return KuruTypes.COMMENT; }
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

{WHITE_SPACE}+                                              { yybegin(YYINITIAL); return TokenType.WHITE_SPACE; }
 
.                                                           { return TokenType.BAD_CHARACTER; }