package org.kurulang.plugin;

import com.intellij.codeInsight.completion.CompletionContributor;
import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.codeInsight.completion.CompletionProvider;
import com.intellij.codeInsight.completion.CompletionResultSet;
import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.util.ProcessingContext;
import org.jetbrains.annotations.NotNull;
import org.kurulang.plugin.psi.KuruTypes;

public class KuruCompletionContributor extends CompletionContributor {
  public KuruCompletionContributor() {
    /* extend(CompletionType.BASIC,
        PlatformPatterns.psiElement(KuruTypes.KIDENT).withLanguage(KuruLanguage.INSTANCE),
        new CompletionProvider<CompletionParameters>() {
          @Override
          protected void addCompletions(@NotNull CompletionParameters completionParameters,
              ProcessingContext processingContext,
              @NotNull CompletionResultSet completionResultSet) {
            System.out.println("Completion");
            completionResultSet.addElement(LookupElementBuilder.create("foobar"));
            completionResultSet.addElement(LookupElementBuilder.create("bar"));
          }
        }); */
  }
}