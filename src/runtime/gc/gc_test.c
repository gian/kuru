#include "gc.h"
#include "typeinfo.h"
#include <stdlib.h>
#include <stdio.h>

struct example_t {
	int *i;
	double *d;
	struct example_t *next;
	struct example_t *loop;
};

typeinfo_t typeinfo_example_t = {
	.name = "example_t",
	.alloc_offset = ALLOC_OFFSET(struct example_t),
	.elements = 4,
	.element = {
		{ offsetof(struct example_t, i), &typeinfo_int },
		{ offsetof(struct example_t, d), &typeinfo_double },
		{ offsetof(struct example_t, next), &typeinfo_example_t },
		{ offsetof(struct example_t, loop), &typeinfo_example_t }
	}
};

int main(int argc, char *argv[])
{
	int *i = gc_malloc(&typeinfo_int);
	double *d = gc_malloc(&typeinfo_double);
	struct example_t *e = gc_malloc(&typeinfo_example_t);

	*i = 1;
	*d = 2.0;

	e->i = i;
	e->d = d;
	e->next = NULL;
	e->loop = e;
	
	printf("adding root\n");
	gc_add_root(e, &typeinfo_example_t);
	printf("gc()\n");
	gc();
	printf("removing root\n");
	gc_rem_root(e, &typeinfo_example_t);
	printf("gc()\n");
	gc();
	printf("done.\n");
	return 0;
}
