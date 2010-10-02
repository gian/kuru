BIN=../../bin
COMPILER=mlton -profile time
KUPEG=$(BIN)/kupeg

all: $(BIN)/kuruc
	make -C frontend

$(BIN)/kuruc: frontend kuruc.mlb main.sml
	$(COMPILER) -output $(BIN)/kuruc kuruc.mlb

main.sml: main.k
	cp main.k main.sml

frontend: force
	make -C frontend

clean:
	rm -f $(BIN)/kuruc main.sml
	make -C frontend clean

force:
	true

