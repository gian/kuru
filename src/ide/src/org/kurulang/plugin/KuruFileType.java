package org.kurulang.plugin;

import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.util.Icons;
import javax.swing.Icon;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class KuruFileType extends LanguageFileType {
  public static final KuruFileType INSTANCE = new KuruFileType();

  private KuruFileType() {
    super(KuruLanguage.INSTANCE);
  }

  @NotNull @Override public String getName() {
    return "Kuru file";
  }

  @NotNull @Override public String getDescription() {
    return "Kuru Programming Language file";
  }

  @NotNull @Override public String getDefaultExtension() {
    return "k";
  }

  @Nullable @Override public Icon getIcon() {
    return Icons.FUNCTION_ICON;
  }
}
