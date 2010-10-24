BIN=../../bin
COMPILER=mlton -profile time
KUPEG=$(BIN)/kupeg

all: $(BIN)/kuruc

tests: $(BIN)/kuruc
	make -C tests

$(BIN)/kuruc: lib semant frontend ast intermediate compile.sml debug.sml kuruc.mlb main.sml 
	./version.sh > version.k
	cp version.k version.sml
	$(COMPILER) -output $(BIN)/kuruc kuruc.mlb

main.sml: main.k
	cp main.k main.sml

compile.sml: compile.k
	cp compile.k compile.sml

debug.sml: debug.k
	cp debug.k debug.sml

frontend: force
	make -C frontend

lib: force
	make -C lib

semant: force
	make -C semant

ast: force
	make -C ast

intermediate: force
	make -C intermediate

clean:
	rm -f $(BIN)/kuruc main.sml compile.sml debug.sml version.sml 
	make -C frontend clean
	make -C lib clean
	make -C ast clean
	make -C semant clean
	make -C intermediate clean

force:
	true

