CFLAGS=-g -Wall -O2 -Wextra -Wstrict-prototypes -pthread
LDLIBS=-lpthread

LIBKURU= \
	libkuru.a(channels.o) \
	libkuru.a(kuruio.o)

all: $(LIBKURU)


test_bin: LDLIBS+=-lkuru
test_bin: LDFLAGS=-L.
test_bin: \
	io_test

io_test: io_test.c $(LIBKURU)

clean:
	rm -f libkuru.a io_test
