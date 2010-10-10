all: unify.sml constrain.sml env.sml 

constrain.sml: constrain.k
	cp constrain.k constrain.sml

env.sml: env.k
	cp env.k env.sml

unify.sml: unify.k
	cp unify.k unify.sml

clean:
	rm -f constrain.sml env.sml 
