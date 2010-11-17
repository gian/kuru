BIN=../../bin
COMPILER=mlton -profile time
KUPEG=$(BIN)/kupeg

SMLDEPS= \
	lib/set.sml lib/table.sml lib/symbol.sml \
	semant/unify.sml semant/constrain.sml semant/env.sml \
	frontend/kuru.kpg.sml frontend/parsetree.sml frontend/error.sml \
 	ast/typed_ast.sml ast/ast_parse_tree.sml ast/ast_app_apply.sml \
	ast/type.sml ast/ast_datatype_unfold.sml ast/ast_infix_apply.sml \
	ast/ast_typedec_eval.sml ast/ast_fun_curry.sml \
	ast/ast_infix_unflatten.sml ast/ast_list_cons.sml \
	ast/ast_match_compile.sml ast/convert_parse_tree.sml \
	semant/typecheck.sml semant/tyconstraints.sml ast/ast_promote_exns.sml \
	semant/monomorphise.sml semant/polymorphism.sml ast/ast_infix_pattern.sml \
	ast/closure_convert.sml semant/typeclass.sml \
	intermediate/temp.sml intermediate/intermediate.sml \
	intermediate/translate.sml codegen/codegen.sml codegen/codegen_decs.sml \
	intermediate/builtins.sml codegen/llvm_operations.sml semant/exceptions.sml \
	config.sml debug.sml main.sml compile.sml version.sml

all: $(BIN)/kuruc

tests: $(BIN)/kuruc
	$(MAKE) -C tests

$(BIN)/kuruc: $(SMLDEPS) kuruc.mlb
	./version.sh > version.k
	$(COMPILER) -output $(BIN)/kuruc kuruc.mlb

frontend/kuru.kpg.k: frontend/kuru.kpg
	$(MAKE) -C frontend kuru.kpg.k

%.sml: %.k
	ln -sf $(notdir $<) $@

version.k: version.sh
	./version.sh > version.k

frontend: 
	$(MAKE) -C frontend

lib: 
	$(MAKE) -C lib

semant: 
	$(MAKE) -C semant

ast: 
	$(MAKE) -C ast

intermediate: 
	$(MAKE) -C intermediate

codegen:
	$(MAKE) -C codegen

clean:
	rm -f $(BIN)/kuruc config.sml main.sml compile.sml debug.sml version.sml 
	$(MAKE) -C frontend clean
	$(MAKE) -C lib clean
	$(MAKE) -C ast clean
	$(MAKE) -C semant clean
	$(MAKE) -C intermediate clean
	$(MAKE) -C codegen clean

.PHONY: clean intermediate ast semant codegen lib frontend 
.INTERMEDIATE: $(SMLDEPS)
