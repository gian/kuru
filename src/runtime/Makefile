CFLAGS=-g -Wall -O2 -Wextra -Wstrict-prototypes -pthread
LDLIBS=-lpthread -lkuru
LDFLAGS=-L$(LIB)
LIB=../../lib

LIBKURU= \
	$(LIB)/libkuru.a(channels.o) \
	$(LIB)/libkuru.a(io_kuru.o) \
	$(LIB)/libkuru.a(kuru_types.o) \
	#$(LIB)/libkuru.a(kuruio.o)

all: $(LIBKURU)

%_kuru.c %_kuru.h %_kuru.ll: %.idl kuruidl
	./kuruidl $< -o $@


clean:
	rm -f $(LIB)/libkuru.a io_test *_kuru.[ch] *_kuru.ll *.o io_test

.SECONDARY: $(wildcard *_kuru.c) $(wildcard *_kuru.h) $(wildcard *_kuru.ll)
