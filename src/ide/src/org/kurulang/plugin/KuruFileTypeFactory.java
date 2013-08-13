package org.kurulang.plugin;

import com.intellij.openapi.fileTypes.FileTypeConsumer;
import com.intellij.openapi.fileTypes.FileTypeFactory;
import org.jetbrains.annotations.NotNull;

public class KuruFileTypeFactory extends FileTypeFactory {
  @Override public void createFileTypes(@NotNull FileTypeConsumer fileTypeConsumer) {
    fileTypeConsumer.consume(KuruFileType.INSTANCE, "k");
  }
}
