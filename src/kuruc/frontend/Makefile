KUPEG=../../../bin/kupeg

all: kuru.kpg.k 

kuru.kpg.k: kuru.kpg $(KUPEG)
	$(KUPEG) kuru.kpg

clean:
	rm -f kuru.kpg.sml kuru.kpg.k parsetree.sml error.sml \
	  parsetree_infix_unflatten.sml parsetree_transformer.sml parsetree_list_cons.sml \
	  parsetree_app_apply.sml parsetree_infix_pattern.sml parsetree_infix_apply.sml parsetree_promote_exns.sml

