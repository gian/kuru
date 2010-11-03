#ifndef GC_H
#define GC_H 1
#include <stddef.h>

typedef struct typeinfo_element_t {
	ptrdiff_t offset;
	struct typeinfo_t *subtype;
} typeinfo_element_t;

typedef struct typeinfo_t {
	const char *name;
	ptrdiff_t alloc_offset;
	int elements;
	typeinfo_element_t element[];
} typeinfo_t;

#define ALIGN_OF(t) \
	((sizeof (t) > 1)? offsetof(struct { char c; typeof(t) x; }, x) : 1)

#define ALLOC_OFFSET(obj) \
	(sizeof(obj) + ALIGN_OF(typeinfo_t) - sizeof(obj) % ALIGN_OF(typeinfo_t))

extern typeinfo_t typeinfo_int;

void *gc_malloc(const typeinfo_t *tinfo);
void gc_add_root(void *ptr, const typeinfo_t *tinfo);
void gc_rem_root(void *ptr, const typeinfo_t *tinfo);
void gc(void);

#endif
