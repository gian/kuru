#include "kuruio.h"

int main(int argc, char *argv[])
{
	(void) argc;
	(void) argv;
	io_init();
	io_puts("Hello World\n");
	io_done();

	return 0;
}
