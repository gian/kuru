BIN=../../bin
COMPILER=mlton

#$(BIN)/kupeg: kupeg.kpg.k main.sml kupeg.mlb kupeg.selfhosted
#	./kupeg.selfhosted -v kupeg.kpg >/dev/null
#	cp kupeg.kpg.k kupeg.kpg.sml
#	$(COMPILER) -output $(BIN)/kupeg kupeg.mlb
#	echo "KuPEG built... self-building final version"
#	$(BIN)/kupeg -v kupeg.kpg >/dev/null
#	cp kupeg.kpg.k kupeg.kpg.sml
#	$(COMPILER) -output $(BIN)/kupeg kupeg.mlb

$(BIN)/kupeg: kupeg-first.kpg.k main.sml kupeg.mlb kupeg.first
	cp kupeg-first.kpg.k kupeg.kpg.sml
	$(COMPILER) -output $(BIN)/kupeg kupeg.mlb

kupeg-first.kpg.k: kupeg.first kupeg.kpg
	./kupeg.first -v kupeg.kpg >/dev/null
	mv -f kupeg.kpg.k kupeg-first.kpg.k

kupeg.first: kupeg-selfhosted.kpg.k kupeg.selfhosted main.sml kupeg.mlb
	cp kupeg-selfhosted.kpg.k kupeg.kpg.sml
	$(COMPILER) -output kupeg.first kupeg.mlb

kupeg-selfhosted.kpg.k: kupeg.selfhosted kupeg.kpg
	./kupeg.selfhosted kupeg.kpg
	mv -f kupeg.kpg.k kupeg-selfhosted.kpg.k

kupeg.selfhosted: kupeg-bootstrap.kpg.k main.sml kupeg.mlb
	cp kupeg-bootstrap.kpg.k kupeg.kpg.sml
	$(COMPILER) -output kupeg.selfhosted kupeg.mlb 

kupeg-bootstrap.kpg.k: kupeg.bootstrap kupeg.kpg
	./kupeg.bootstrap kupeg.kpg
	mv -f kupeg.kpg.k kupeg-bootstrap.kpg.k

kupeg.bootstrap: bootstrap.sml
	$(COMPILER) -output kupeg.bootstrap bootstrap.sml

.PRECIOUS: kupeg.kpg.k

clean:
	rm -f $(BIN)/kupeg \
		kupeg.bootstrap kupeg.selfhosted kupeg.first \
		kupeg-bootstrap.kpg.k kupeg-selfhosted.kpg.k kupeg-first.kpg.k\
		kupeg.kpg.sml kupeg.kpg.k kupeg.sml

.PHONY: clean

.INTERMEDIATE: kupeg.bootstrap kupeg.selfhosted kupeg.first \
		kupeg-bootstrap.kpg.k kupeg-selfhosted.kpg.k kupeg-first.kpg.k
	
