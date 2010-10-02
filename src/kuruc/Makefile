BIN=../../bin
COMPILER=mlton -profile time
KUPEG=$(BIN)/kupeg

all: $(BIN)/kuruc

$(BIN)/kuruc: lib semant frontend kuruc.mlb main.sml
	$(COMPILER) -output $(BIN)/kuruc kuruc.mlb

main.sml: main.k
	cp main.k main.sml

frontend: force
	make -C frontend

lib: force
	make -C lib

semant: force
	make -C semant

clean:
	rm -f $(BIN)/kuruc main.sml
	make -C frontend clean

force:
	true

