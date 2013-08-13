package org.kurulang.plugin.psi;

import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.kurulang.plugin.KuruLanguage;

public class KuruTokenType extends IElementType {
  public KuruTokenType(@NotNull @NonNls String debugName) {
    super(debugName, KuruLanguage.INSTANCE);
  }

  @Override public String toString() {
    return "KuruTokenType." + super.toString();
  }
}
