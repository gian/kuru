BIN=../../bin
COMPILER=mlton
KUPEG=$(BIN)/kupeg

SMLDEPS= \
	parse.sml docgen.sml parsetree.sml main.sml kdoc.kpg.sml

all: $(BIN)/kdoc

tests: $(BIN)/kdoc
	$(MAKE) -C tests

$(BIN)/kdoc: $(SMLDEPS) kdoc.mlb
	$(COMPILER) -output $(BIN)/kdoc kdoc.mlb

kdoc.kpg.k: kdoc.kpg
	$(KUPEG) kdoc.kpg

%.sml: %.k
	ln -sf $(notdir $<) $@

clean:
	rm -f $(BIN)/kdoc main.sml docgen.sml parse.sml parsetree.sml kdoc.kpg.sml kdoc.kpg.k 

.INTERMEDIATE: $(SMLDEPS)
