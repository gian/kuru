KUPEG=../../../bin/kupeg

all: kuru.kpg.sml parsetree.sml error.sml

kuru.kpg.sml: kuru.kpg.k
	cp kuru.kpg.k kuru.kpg.sml

parsetree.sml: parsetree.k
	cp parsetree.k parsetree.sml

error.sml: error.k
	cp error.k error.sml

kuru.kpg.k: kuru.kpg
	$(KUPEG) kuru.kpg

clean:
	rm -f kuru.kpg.sml kuru.kpg.k parsetree.sml error.sml

