BIN=../../bin
COMPILER=mlton -profile time
KUPEG=$(BIN)/kupeg

all: $(BIN)/kuruc

tests: $(BIN)/kuruc
	make -C tests

$(BIN)/kuruc: lib semant frontend ast debug.sml kuruc.mlb main.sml version.sml
	$(COMPILER) -output $(BIN)/kuruc kuruc.mlb

main.sml: main.k
	cp main.k main.sml

debug.sml: debug.k
	cp debug.k debug.sml

version.sml: version.k
	cp version.k version.sml

version.k: force
	./version.sh > version.k

frontend: force
	make -C frontend

lib: force
	make -C lib

semant: force
	make -C semant

ast: force
	make -C ast

clean:
	rm -f $(BIN)/kuruc main.sml debug.sml version.sml version.k 
	make -C frontend clean
	make -C lib clean
	make -C ast clean
	make -C semant clean

force:
	true

