BIN=../../bin
COMPILER=mlton

$(BIN)/kupeg: kupeg.sml
	$(COMPILER) -output $(BIN)/kupeg kupeg.sml

kupeg.sml: kupeg.kpg.k end.k preample.k
	cat preample.k kupeg.kpg.k end.k > kupeg.sml

kupeg.kpg.k: kupeg.bootstrap kupeg.kpg
	./kupeg.bootstrap kupeg.kpg

kupeg.bootstrap: bootstrap.sml
	$(COMPILER) -output kupeg.bootstrap bootstrap.sml

clean:
	rm -f $(BIN)/kupeg kupeg.bootstrap kupeg.kpg.k kupeg.sml
