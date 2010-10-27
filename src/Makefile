all: kupeg kil kulex kuruc kuyacc tests

kupeg:
	$(MAKE) -C kupeg

kulex:
	$(MAKE) -C kulex

kuruc: kupeg
	$(MAKE) -C kuruc

kuruacc:
	$(MAKE) -C kuyacc

tests: kuruc kuruacc
	$(MAKE) -C tests

clean:
	$(MAKE) -C kupeg clean
	$(MAKE) -C kulex clean
	$(MAKE) -C kuruc clean
	$(MAKE) -C kuyacc clean
	$(MAKE) -C tests clean


.PHONY: tests clean kuruacc kuruc kulex kupeg

