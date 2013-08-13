package org.kurulang.plugin;

import com.intellij.lexer.FlexAdapter;
import java.io.Reader;

/**
 * Created with IntelliJ IDEA. User: gian Date: 8/12/13 Time: 4:43 PM To change this template use
 * File | Settings | File Templates.
 */
public class KuruLexerAdapter extends FlexAdapter {
  public KuruLexerAdapter() {
    super(new KuruLexer((Reader) null));
  }
}
