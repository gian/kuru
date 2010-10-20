all: typed_ast.sml ast_parse_tree.sml ast_app_apply.sml type.sml ast_datatype_unfold.sml ast_infix_apply.sml ast_typedec_eval.sml ast_fun_curry.sml ast_match_compile.sml convert_parse_tree.sml

typed_ast.sml: typed_ast.k
	cp typed_ast.k typed_ast.sml

ast_parse_tree.sml: ast_parse_tree.k
	cp ast_parse_tree.k ast_parse_tree.sml

ast_app_apply.sml: ast_app_apply.k
	cp ast_app_apply.k ast_app_apply.sml

type.sml: type.k
	cp type.k type.sml

ast_datatype_unfold.sml: ast_datatype_unfold.k
	cp ast_datatype_unfold.k ast_datatype_unfold.sml

ast_infix_apply.sml: ast_infix_apply.k
	cp ast_infix_apply.k ast_infix_apply.sml

ast_typedec_eval.sml: ast_typedec_eval.k
	cp ast_typedec_eval.k ast_typedec_eval.sml

ast_fun_curry.sml: ast_fun_curry.k
	cp ast_fun_curry.k ast_fun_curry.sml

ast_match_compile.sml: ast_match_compile.k
	cp ast_match_compile.k ast_match_compile.sml

convert_parse_tree.sml: convert_parse_tree.k
	cp convert_parse_tree.k convert_parse_tree.sml

clean:
	rm -f ast_datatype_unfold.sml ast_fun_curry.sml ast_app_apply.sml ast_parse_tree.sml type.sml ast_infix_apply.sml ast_match_compile.sml ast_typedec_eval.sml convert_parse_tree.sml typed_ast.sml
