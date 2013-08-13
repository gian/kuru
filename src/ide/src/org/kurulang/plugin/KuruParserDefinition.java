package org.kurulang.plugin;

import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.ParserDefinition;
import com.intellij.lang.PsiParser;
import com.intellij.lexer.FlexAdapter;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.TokenSet;
import java.io.Reader;
import org.jetbrains.annotations.NotNull;
import org.kurulang.plugin.parser.KuruParser;
import org.kurulang.plugin.psi.KuruFile;
import org.kurulang.plugin.psi.KuruTypes;

/**
 * Created with IntelliJ IDEA. User: gian Date: 8/12/13 Time: 4:27 PM To change this template use
 * File | Settings | File Templates.
 */
public class KuruParserDefinition implements ParserDefinition {
  public static final TokenSet WHITE_SPACES = TokenSet.create(TokenType.WHITE_SPACE);
  public static final TokenSet COMMENTS = TokenSet.create(KuruTypes.COMMENT);
  public static final TokenSet STRINGS = TokenSet.create(KuruTypes.STRING);

  public static final IFileElementType FILE = new IFileElementType(
      Language.<KuruLanguage>findInstance(KuruLanguage.class));

  @NotNull @Override public Lexer createLexer(Project project) {
    return new FlexAdapter(new KuruLexer((Reader) null));
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
  public SpaceRequirements spaceExistanceTypeBetweenTokens(ASTNode astNode,
      ASTNode astNode2) {
    return SpaceRequirements.MAY;
  }
}
