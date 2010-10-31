#include "kuruio.h"

int main(int argc, char *argv[])
{
	(void) argc;
	(void) argv;
	io_init();
	kuru_string_t s;
	s.length = 13;
	s.data = "Hello World\n";
	io_puts(s);
	io_done();

	return 0;
}
