package org.kurulang.plugin;

import com.intellij.lang.Language;

public class KuruLanguage extends Language {
  public static final KuruLanguage INSTANCE = new KuruLanguage();

  private KuruLanguage() {
    super("Kuru");
  }
}
