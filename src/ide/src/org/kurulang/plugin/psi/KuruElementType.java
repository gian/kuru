package org.kurulang.plugin.psi;

import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.kurulang.plugin.KuruLanguage;

public class KuruElementType extends IElementType {
  public KuruElementType(@NotNull @NonNls String debugName) {
    super(debugName, KuruLanguage.INSTANCE);
  }
}
