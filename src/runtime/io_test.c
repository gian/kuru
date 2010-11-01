#include "io_kuru.h"

int main(int argc, char *argv[])
{
	io_init();
	k_io_print(c2ks("Press enter"));
	int ch = k_io_getch();
	if (ch=='\n')
		k_io_print(c2ks("Thank you\n"));
	else
		k_io_print(c2ks("You cannot follow instructions\n"));
	io_done();
	return 0;
}
