BIN=../../bin
COMPILER=mlton

$(BIN)/kupeg: kupeg.kpg.sml main.sml kupeg.mlb kupeg.selfhosted
	./kupeg.selfhosted kupeg.kpg
	cp kupeg.kpg.k kupeg.kpg.sml
	$(COMPILER) -output $(BIN)/kupeg kupeg.mlb
	echo "KuPEG built... self-building final version"
	$(BIN)/kupeg kupeg.kpg
	cp kupeg.kpg.k kupeg.kpg.sml
	$(COMPILER) -output $(BIN)/kupeg kupeg.mlb

kupeg.selfhosted: kupeg.kpg.sml main.sml kupeg.mlb
	$(COMPILER) -output kupeg.selfhosted kupeg.mlb

kupeg.kpg.sml: kupeg.kpg.k 
	cp kupeg.kpg.k kupeg.kpg.sml

kupeg.kpg.k: kupeg.bootstrap kupeg.kpg
	./kupeg.bootstrap kupeg.kpg

kupeg.bootstrap: bootstrap.sml
	$(COMPILER) -output kupeg.bootstrap bootstrap.sml

clean:
	rm -f $(BIN)/kupeg kupeg.bootstrap kupeg.selfhosted kupeg.kpg.sml kupeg.kpg.k kupeg.sml
