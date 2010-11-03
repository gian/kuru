CFLAGS=-g -Wall -Wextra -Wstrict-prototypes -O2

all: gc.o gc_test

gc_test: gc_test.c gc.o typeinfo.o

clean:
	rm -f *.o gc_test
