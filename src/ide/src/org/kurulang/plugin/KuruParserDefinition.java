package org.kurulang.plugin;

import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.ParserDefinition;
import com.intellij.lang.PsiParser;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.TokenSet;
import org.jetbrains.annotations.NotNull;
import org.kurulang.plugin.parser.Kuru;
import org.kurulang.plugin.parser.KuruParser;
import org.kurulang.plugin.parser.KuruTokenTypes;
import org.kurulang.plugin.psi.KuruFile;
import org.kurulang.plugin.psi.KuruTypes;

public class KuruParserDefinition implements ParserDefinition {
  public static final TokenSet WHITE_SPACES = TokenSet.create(TokenType.WHITE_SPACE);
  public static final TokenSet COMMENTS = TokenSet.create(KuruTokenTypes.COMMENT_START, KuruTokenTypes.COMMENT_CHAR, KuruTokenTypes.COMMENT_END, KuruTokenTypes.COMMENT);
  public static final TokenSet STRINGS = TokenSet.create(KuruTokenTypes.STRING);

  public static final IFileElementType FILE = new IFileElementType(
      Language.<KuruLanguage>findInstance(KuruLanguage.class));

  @NotNull @Override public Lexer createLexer(Project project) {
    return new Kuru();
  }

  @Override public PsiParser createParser(Project project) {
    return new KuruParser();
  }

  @Override public IFileElementType getFileNodeType() {
    return FILE;
  }

  @NotNull @Override public TokenSet getWhitespaceTokens() {
    return WHITE_SPACES;
  }

  @NotNull @Override public TokenSet getCommentTokens() {
    return COMMENTS;
  }

  @NotNull @Override public TokenSet getStringLiteralElements() {
    return STRINGS;
  }

  @NotNull @Override public PsiElement createElement(ASTNode astNode) {
    return KuruTypes.Factory.createElement(astNode);
  }

  @Override public PsiFile createFile(FileViewProvider viewProvider) {
    return new KuruFile(viewProvider);
  }

  @Override
  public SpaceRequirements spaceExistanceTypeBetweenTokens(ASTNode astNode, ASTNode astNode2) {
    return SpaceRequirements.MAY;
  }
}
