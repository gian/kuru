BIN=../../bin
COMPILER=mlton
KUPEG=$(BIN)/kupeg

all: $(BIN)/kuruc

$(BIN)/kuruc: kuruc.mlb main.sml kuru.kpg.sml
	$(COMPILER) -output $(BIN)/kuruc kuruc.mlb

main.sml: main.k
	cp main.k main.sml

kuru.kpg.sml: kuru.kpg.k
	cp kuru.kpg.k kuru.kpg.sml

kuru.kpg.k: kuru.kpg
	$(KUPEG) kuru.kpg

clean:
	rm -f $(BIN)/kuruc main.sml kuru.kpg.sml kuru.kpg.k

