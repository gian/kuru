all: kupeg kil kdoc kuruc runtime tests

kupeg:
	$(MAKE) -C kupeg

kuruc: kupeg
	$(MAKE) -C kuruc

tests: kuruc kil 
	$(MAKE) -C tests
	$(MAKE) -C kuruc tests

runtime:
	$(MAKE) -C runtime

kdoc:
	$(MAKE) -C kdoc

clean:
	$(MAKE) -C kupeg clean
	$(MAKE) -C kuruc clean
	$(MAKE) -C tests clean
	$(MAKE) -C runtime clean
	$(MAKE) -C kdoc clean

.PHONY: tests clean kdoc kuruc kupeg runtime

