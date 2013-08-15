package org.kurulang.plugin.parser;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import org.kurulang.plugin.psi.KuruElementType;
import org.kurulang.plugin.psi.KuruTokenType;
import org.kurulang.plugin.psi.impl.KuruConImpl;
import org.kurulang.plugin.psi.impl.KuruConbindImpl;
import org.kurulang.plugin.psi.impl.KuruDatabindImpl;
import org.kurulang.plugin.psi.impl.KuruDecImpl;
import org.kurulang.plugin.psi.impl.KuruExpImpl;
import org.kurulang.plugin.psi.impl.KuruFunbindImpl;
import org.kurulang.plugin.psi.impl.KuruFunmatchImpl;
import org.kurulang.plugin.psi.impl.KuruLongidImpl;
import org.kurulang.plugin.psi.impl.KuruMatchImpl;
import org.kurulang.plugin.psi.impl.KuruPatImpl;
import org.kurulang.plugin.psi.impl.KuruProgramelImpl;
import org.kurulang.plugin.psi.impl.KuruSdecImpl;
import org.kurulang.plugin.psi.impl.KuruSigbindImpl;
import org.kurulang.plugin.psi.impl.KuruSignImpl;
import org.kurulang.plugin.psi.impl.KuruStrImpl;
import org.kurulang.plugin.psi.impl.KuruStrdescImpl;
import org.kurulang.plugin.psi.impl.KuruStringLiteralImpl;
import org.kurulang.plugin.psi.impl.KuruTypImpl;
import org.kurulang.plugin.psi.impl.KuruTypebindImpl;
import org.kurulang.plugin.psi.impl.KuruTypedescImpl;
import org.kurulang.plugin.psi.impl.KuruTyvarlistImpl;
import org.kurulang.plugin.psi.impl.KuruTyvarsImpl;
import org.kurulang.plugin.psi.impl.KuruValbindImpl;
import org.kurulang.plugin.psi.impl.KuruValdescImpl;

/**
 * Created with IntelliJ IDEA. User: gian Date: 8/14/13 Time: 9:20 PM To change this template use
 * File | Settings | File Templates.
 */
public interface KuruTokenTypes {

  IElementType CON = new KuruElementType("CON");
  IElementType CONBIND = new KuruElementType("CONBIND");
  IElementType DATABIND = new KuruElementType("DATABIND");
  IElementType DEC = new KuruElementType("DEC");
  IElementType EXP = new KuruElementType("EXP");
  IElementType FUNBIND = new KuruElementType("FUNBIND");
  IElementType FUNMATCH = new KuruElementType("FUNMATCH");
  IElementType LONGID = new KuruElementType("LONGID");
  IElementType MATCH = new KuruElementType("MATCH");
  IElementType PAT = new KuruElementType("PAT");
  IElementType PROGRAMEL = new KuruElementType("PROGRAMEL");
  IElementType SDEC = new KuruElementType("SDEC");
  IElementType SIGBIND = new KuruElementType("SIGBIND");
  IElementType SIGN = new KuruElementType("SIGN");
  IElementType STR = new KuruElementType("STR");
  IElementType STRDESC = new KuruElementType("STRDESC");
  IElementType STRING_LITERAL = new KuruElementType("STRING_LITERAL");
  IElementType TYP = new KuruElementType("TYP");
  IElementType TYPEBIND = new KuruElementType("TYPEBIND");
  IElementType TYPEDESC = new KuruElementType("TYPEDESC");
  IElementType TYVARLIST = new KuruElementType("TYVARLIST");
  IElementType TYVARS = new KuruElementType("TYVARS");
  IElementType VALBIND = new KuruElementType("VALBIND");
  IElementType VALDESC = new KuruElementType("VALDESC");

  IElementType ABSTYPE = new KuruTokenType("abstype");
  IElementType AND = new KuruTokenType("and");
  IElementType ANDALSO = new KuruTokenType("andalso");
  IElementType ARROW = new KuruTokenType("->");
  IElementType AS = new KuruTokenType("as");
  IElementType BAR = new KuruTokenType("|");
  IElementType CASE = new KuruTokenType("case");
  IElementType COLON = new KuruTokenType(":");
  IElementType COLONGT = new KuruTokenType(":>");
  IElementType COMMA = new KuruTokenType(",");
  IElementType DARROW = new KuruTokenType("=>");
  IElementType DATATYPE = new KuruTokenType("datatype");
  IElementType DO = new KuruTokenType("do");
  IElementType DOTDOTDOT = new KuruTokenType("...");
  IElementType ELSE = new KuruTokenType("else");
  IElementType END = new KuruTokenType("end");
  IElementType EQTYPE = new KuruTokenType("eqtype");
  IElementType EQUALOP = new KuruTokenType("=");
  IElementType EXCEPTION = new KuruTokenType("exception");
  IElementType FN = new KuruTokenType("fn");
  IElementType FUN = new KuruTokenType("fun");
  IElementType FUNCTOR = new KuruTokenType("functor");
  IElementType HANDLE = new KuruTokenType("handle");
  IElementType HASH = new KuruTokenType("#");
  IElementType IF = new KuruTokenType("if");
  IElementType IN = new KuruTokenType("in");
  IElementType INCLUDE = new KuruTokenType("include");
  IElementType INFIX = new KuruTokenType("infix");
  IElementType INFIXR = new KuruTokenType("infixr");
  IElementType KEYWORD = new KuruTokenType("KEYWORD");
  IElementType KIDENT = new KuruTokenType("KIDENT");
  IElementType LBRACE = new KuruTokenType("{");
  IElementType LBRACKET = new KuruTokenType("[");
  IElementType LET = new KuruTokenType("let");
  IElementType LOCAL = new KuruTokenType("local");
  IElementType LPAREN = new KuruTokenType("(");
  IElementType NONFIX = new KuruTokenType("nonfix");
  IElementType NUM = new KuruTokenType("NUM");
  IElementType OF = new KuruTokenType("of");
  IElementType OP = new KuruTokenType("op");
  IElementType OPEN = new KuruTokenType("open");
  IElementType ORELSE = new KuruTokenType("orelse");
  IElementType RAISE = new KuruTokenType("raise");
  IElementType RBRACE = new KuruTokenType("}");
  IElementType RBRACKET = new KuruTokenType("]");
  IElementType REC = new KuruTokenType("rec");
  IElementType RPAREN = new KuruTokenType(")");
  IElementType SEMICOLON = new KuruTokenType(";");
  IElementType SHARING = new KuruTokenType("sharing");
  IElementType SIG = new KuruTokenType("sig");
  IElementType SIGNATURE = new KuruTokenType("signature");
  IElementType STRING = new KuruTokenType("STRING");
  IElementType STRUCT = new KuruTokenType("struct");
  IElementType STRUCTURE = new KuruTokenType("structure");
  IElementType THEN = new KuruTokenType("then");
  IElementType TYPE = new KuruTokenType("type");
  IElementType TYVAR = new KuruTokenType("tyvar");
  IElementType VAL = new KuruTokenType("val");
  IElementType WHERE = new KuruTokenType("where");
  IElementType WHILE = new KuruTokenType("while");
  IElementType WHITE_SPACE = new KuruTokenType("WHITE_SPACE");
  IElementType WILD = new KuruTokenType("_");
  IElementType WITH = new KuruTokenType("with");
  IElementType WITHTYPE = new KuruTokenType("withtype");
  IElementType COMMENT_START = new KuruTokenType("COMMENT_START");
  IElementType COMMENT_CHAR = new KuruTokenType("COMMENT_CHAR");
  IElementType COMMENT_END = new KuruTokenType("COMMENT_END");
  IElementType COMMENT = new KuruTokenType("COMMENT");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
      if (type == CON) {
        return new KuruConImpl(node);
      }
      else if (type == CONBIND) {
        return new KuruConbindImpl(node);
      }
      else if (type == DATABIND) {
        return new KuruDatabindImpl(node);
      }
      else if (type == DEC) {
        return new KuruDecImpl(node);
      }
      else if (type == EXP) {
        return new KuruExpImpl(node);
      }
      else if (type == FUNBIND) {
        return new KuruFunbindImpl(node);
      }
      else if (type == FUNMATCH) {
        return new KuruFunmatchImpl(node);
      }
      else if (type == LONGID) {
        return new KuruLongidImpl(node);
      }
      else if (type == MATCH) {
        return new KuruMatchImpl(node);
      }
      else if (type == PAT) {
        return new KuruPatImpl(node);
      }
      else if (type == PROGRAMEL) {
        return new KuruProgramelImpl(node);
      }
      else if (type == SDEC) {
        return new KuruSdecImpl(node);
      }
      else if (type == SIGBIND) {
        return new KuruSigbindImpl(node);
      }
      else if (type == SIGN) {
        return new KuruSignImpl(node);
      }
      else if (type == STR) {
        return new KuruStrImpl(node);
      }
      else if (type == STRDESC) {
        return new KuruStrdescImpl(node);
      }
      else if (type == STRING_LITERAL) {
        return new KuruStringLiteralImpl(node);
      }
      else if (type == TYP) {
        return new KuruTypImpl(node);
      }
      else if (type == TYPEBIND) {
        return new KuruTypebindImpl(node);
      }
      else if (type == TYPEDESC) {
        return new KuruTypedescImpl(node);
      }
      else if (type == TYVARLIST) {
        return new KuruTyvarlistImpl(node);
      }
      else if (type == TYVARS) {
        return new KuruTyvarsImpl(node);
      }
      else if (type == VALBIND) {
        return new KuruValbindImpl(node);
      }
      else if (type == VALDESC) {
        return new KuruValdescImpl(node);
      }
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
