package org.kurulang.plugin;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.SyntaxHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import java.awt.Color;
import java.awt.Font;
import org.jetbrains.annotations.NotNull;
import org.kurulang.plugin.parser.Kuru;
import org.kurulang.plugin.parser.KuruTokenTypes;

import static com.intellij.openapi.editor.colors.TextAttributesKey.createTextAttributesKey;

public class KuruSyntaxHighlighter extends SyntaxHighlighterBase {
  public static final TextAttributesKey KEYWORD =
      createTextAttributesKey("KURU_KEYWORD", SyntaxHighlighterColors.KEYWORD);
  public static final TextAttributesKey STRING = createTextAttributesKey(
      "KURU_STRING", SyntaxHighlighterColors.STRING);
  public static final TextAttributesKey LITERAL = createTextAttributesKey(
      "KURU_CON", SyntaxHighlighterColors.NUMBER);
  public static final TextAttributesKey COMMENT = createTextAttributesKey(
      "KURU_COMMENT", SyntaxHighlighterColors.JAVA_BLOCK_COMMENT);

  static final TextAttributesKey BAD_CHARACTER = createTextAttributesKey("KURU_BAD_CHARACTER",
      new TextAttributes(Color.RED, null, null, null, Font.BOLD));

  private static final TextAttributesKey[] BAD_CHAR_KEYS = new TextAttributesKey[] {BAD_CHARACTER};
  private static final TextAttributesKey[] KEYWORD_KEYS = new TextAttributesKey[] {KEYWORD};
  private static final TextAttributesKey[] STRING_KEYS = new TextAttributesKey[] {STRING};
  private static final TextAttributesKey[] COMMENT_KEYS = new TextAttributesKey[] {COMMENT};
  private static final TextAttributesKey[] LITERAL_KEYS = new TextAttributesKey[] {LITERAL};
  private static final TextAttributesKey[] EMPTY_KEYS = new TextAttributesKey[0];

  @NotNull @Override public Lexer getHighlightingLexer() {
    return new Kuru();
  }

  @NotNull @Override public TextAttributesKey[] getTokenHighlights(IElementType tokenType) {
    if (tokenType.equals(KuruTokenTypes.COMMENT_START) || tokenType.equals(
        KuruTokenTypes.COMMENT_CHAR) || tokenType.equals(KuruTokenTypes.COMMENT_END)) {
      return COMMENT_KEYS;
    } else if (tokenType.equals(KuruTokenTypes.KEYWORD)) {
      return KEYWORD_KEYS;
    } else if (tokenType.equals(KuruTokenTypes.STRING)) {
      return STRING_KEYS;
    } else if (tokenType.equals(KuruTokenTypes.CON)) {
      return LITERAL_KEYS;
    } else if (tokenType.equals(TokenType.BAD_CHARACTER)) {
      return BAD_CHAR_KEYS;
    } else {
      return EMPTY_KEYS;
    }
  }
}
