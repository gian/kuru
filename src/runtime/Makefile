CFLAGS=-g -Wall -O2 -Wextra -Wstrict-prototypes -pthread
LDLIBS=-lpthread -lkuru
LDFLAGS=-L$(LIB)
LIB=../../lib

LIBKURU= \
	$(LIB)/libkuru.a(channels.o) \
	$(LIB)/libkuru.a(kuru_types.o) \
	$(LIB)/libkuru.a(io_kuru.o) \
	$(LIB)/libkuru.a(int_kuru.o) \
	$(LIB)/libkuru.a(string_kuru.o) \
	#$(LIB)/libkuru.a(kuruio.o)

all: $(LIBKURU) gc

%_kuru.c %_kuru.h %_kuru.ll %.k: %.idl kuruidl
	./kuruidl $< -o $@

gc:
	make -C gc

clean:
	rm -f $(LIB)/libkuru.a io_test *_kuru.[ch] *_kuru.ll *.o *.k io_test
	make -C gc clean


.PHONY: gc
.SECONDARY: $(wildcard *_kuru.c) \
		$(wildcard *_kuru.h) \
		$(wildcard *_kuru.ll)
