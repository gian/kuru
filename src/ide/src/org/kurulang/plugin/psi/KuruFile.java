package org.kurulang.plugin.psi;

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import org.kurulang.plugin.KuruFileType;
import org.kurulang.plugin.KuruLanguage;

public class KuruFile extends PsiFileBase {
  public KuruFile(@NotNull FileViewProvider viewProvider) {
    super(viewProvider, KuruLanguage.INSTANCE);
  }

  @NotNull @Override public FileType getFileType() {
    return KuruFileType.INSTANCE;
  }

  @Override
  public String toString() {
    return "Kuru File";
  }

  @Override public Icon getIcon(int flags) {
    return super.getIcon(flags);
  }
}
