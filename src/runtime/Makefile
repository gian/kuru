CFLAGS=-g -Wall -O2 -Wextra -Wstrict-prototypes -pthread
LDLIBS=-lpthread
LIB=../../lib

LIBKURU= \
	$(LIB)/libkuru.a(channels.o) \
	$(LIB)/libkuru.a(kuruio.o)

all: $(LIBKURU)

test_bin: LDLIBS+=-lkuru
test_bin: LDFLAGS=-L.
test_bin: \
	io_test

io_test: io_test.c $(LIBKURU)

clean:
	rm -f $(LIB)/libkuru.a io_test
