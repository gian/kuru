#include "gc.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h> /* Debugging */

#define MAGIC 0xCAFEFEED

typedef struct alloc_t {
	unsigned int magic;
	int colour;
	const typeinfo_t *typeinfo;
	/* Points to the next_collectable of the previous structure. */
	struct alloc_t **prev_collectable;
	struct alloc_t *next_collectable;
	struct alloc_t *prev_root;
	struct alloc_t *next_root;
	int root_ref;
} alloc_t;

static int black = 1;
static int grey = 2;


enum { COL_BLACK, COL_GREY, COL_WHITE };

#define COLOUR(alloc) \
	((alloc)->colour == black ? COL_BLACK	\
	: (alloc)->colour == grey ? COL_GREY	\
	: COL_WHITE)

#define ALLOC_TO_PTR(alloc) \
		(void*)(((char*)(alloc)) - (alloc)->typeinfo->alloc_offset)
#define PTR_TO_ALLOC(ptr, typeinfo) \
		(alloc_t*)(((char*)(ptr)) + (typeinfo)->alloc_offset)

#define CHILD(typeinfo, n) \
		((typeinfo)->element[n])

#define CHILD_PTR(ptr, typeinfo, n) \
		*((void **)((char*)(ptr) + CHILD(typeinfo, n).offset))

#define CHILD_TYPEINFO(ptr, typeinfo, n) \
		(CHILD(typeinfo, n).subtype)

static alloc_t *collectable = NULL;
static alloc_t *roots = NULL;
static alloc_t *blackl = NULL;
static alloc_t *greyl = NULL;

static void gc_unlink(alloc_t *alloc)
{
	assert(alloc);
	assert(alloc->magic == MAGIC);
	assert(alloc->prev_collectable);
	assert(alloc->next_collectable != alloc);
	assert(*alloc->prev_collectable == alloc);
	assert(!alloc->next_collectable 
		|| alloc->next_collectable->prev_collectable == &alloc->next_collectable);

	if (alloc->prev_collectable) {
		*(alloc->prev_collectable) = alloc->next_collectable;
	}

	if (alloc->next_collectable) {
		alloc->next_collectable->prev_collectable =
			alloc->prev_collectable;
	}
	alloc->next_collectable = NULL;
	alloc->prev_collectable = NULL;
}

static void gc_link(alloc_t **list, alloc_t *alloc)
{
	/* Make sure that this is actually unlinked. */
	assert(alloc->magic == MAGIC);
	assert(alloc->prev_collectable == NULL);
	assert(alloc->next_collectable == NULL);
	assert(!*list || *(*list)->prev_collectable == *list);
	assert(list);

	alloc->next_collectable = *list;
	alloc->prev_collectable = list;

	if (*list)
		(*list)->prev_collectable = &alloc->next_collectable;
	*list = alloc;

	assert(!alloc->next_collectable 
		|| alloc->next_collectable->prev_collectable == &alloc->next_collectable);

	assert(*alloc->prev_collectable == alloc);
	assert(alloc->next_collectable != alloc);
}

static void gc_move(alloc_t **list, alloc_t *alloc)
{
	gc_unlink(alloc);
	gc_link(list, alloc);
}

void *gc_malloc(const typeinfo_t *tinfo)
{
	void *ret = malloc(tinfo->alloc_offset + sizeof(alloc_t));
	alloc_t *alloc = PTR_TO_ALLOC(ret, tinfo);
	alloc->magic = MAGIC;
	alloc->colour = COL_BLACK;
	alloc->typeinfo = tinfo;
	alloc->prev_collectable = NULL;
	alloc->next_collectable = NULL;
	alloc->next_root = NULL;
	alloc->prev_root = NULL;
	alloc->root_ref = 0;
	gc_link(&collectable, alloc);
	return ret;
}

void gc_add_root(void *ptr, const typeinfo_t *tinfo)
{
	alloc_t *alloc = PTR_TO_ALLOC(ptr, tinfo);
	if (!alloc->next_root) {
		alloc->next_root = roots;
		alloc->prev_root = NULL;
		assert (!alloc->next_root || !alloc->next_root->prev_root);
		if (alloc->next_root)
			alloc->next_root->prev_root = alloc;
		roots = alloc;
		assert(alloc->root_ref == 0);
	}
	++alloc->root_ref;
}

void gc_rem_root(void *ptr, const typeinfo_t *tinfo)
{
	alloc_t *alloc = PTR_TO_ALLOC(ptr, tinfo);
	assert(alloc->root_ref > 0);
	alloc->root_ref--;
	if (alloc->root_ref < 1) {
		if (alloc->next_root)
			alloc->next_root->prev_root = alloc->prev_root;
		if (alloc->prev_root)
			alloc->prev_root->next_root = alloc->next_root;
		else
			roots = alloc->next_root;
		alloc->next_root = alloc->prev_root = NULL;
	}
	/* TODO(perryl): We might want to do a gc() now */
}

static void gc_darken(alloc_t *alloc)
{
	void *ptr = ALLOC_TO_PTR(alloc);
	int i;
	printf("%s@%p has %d out-refs\n",
		alloc->typeinfo->name,
		ptr,
		alloc->typeinfo->elements);
	/* For each child */
	for(i = 0; i < alloc->typeinfo->elements; ++i) {
		void *childp = CHILD_PTR(ptr, alloc->typeinfo, i);
		typeinfo_t *childti = CHILD_TYPEINFO(ptr, alloc->typeinfo, i);

		if (childp == NULL) {
			printf("%s@%p->%s is NULL\n",
				alloc->typeinfo->name,
				ptr,
				childti->name);
			continue;
		}
		
		alloc_t *childalloc = PTR_TO_ALLOC(childp, childti);

		switch (COLOUR(childalloc)) {
			/* Already black?  Well, we're done then. */
			case COL_BLACK:
				printf(" %s@%p->%s@%p is already black.\n",
					alloc->typeinfo->name,
					ptr,
					childti->name,
					childp);
				continue;
			/* Already grey? Done. */
			case COL_GREY:
				printf(" %s@%p->%s@%p is already grey.\n",
					alloc->typeinfo->name,
					ptr,
					childti->name,
					childp);
				continue;
			/* White? Time to change to grey. */
			case COL_WHITE:
				printf(" %s@%p->%s@%p is white\n",
					alloc->typeinfo->name,
					ptr,
					childti->name,
					childp);
				childalloc->colour = grey;
				gc_move(&greyl, childalloc);
				continue;
		}
	}
}

void gc(void)
{
	alloc_t *next = NULL;
	alloc_t *it = NULL;

	assert(blackl == NULL);
	assert(greyl == NULL);

	/* Grab a new set of colours -- this makes old blacks and greys become
	 * white.
	 */
	black += 2;
	grey += 2;

	/* Mark all the roots as black. */
	for (it = roots; it; it=next)
	{
		next=it->next_root;

		it->colour = black;
		
		/* Move it to the black list */
		gc_move(&blackl, it);
		
		printf("%s@%p is black: root.\n", 
			it->typeinfo->name, 
			ALLOC_TO_PTR(it));

		/* Colour all the dependants as grey. */
		gc_darken(it);
	}

	/* Now, lets walk over all the grey nodes until it's empty. */
	while ( greyl ) {
		it = greyl;

		it->colour = black;

		printf("%s@%p is grey->black.\n", 
			it->typeinfo->name, 
			ALLOC_TO_PTR(it));
	
		/* Move to the black list */
		gc_move(&blackl, it);

		/* Colour all unvisited dependants as grey. */
		gc_darken(it);
	}

	assert(greyl == NULL);

	/* Ok, any allocations that haven't been touched aren't grey or black
	 * and thus must be white -- garbage.
	 */
	while ( collectable ) {
		it = collectable;
		gc_unlink(it);
		/* Free it. */
		printf("Freeing %s at %p\n", 
			it->typeinfo->name, 
			ALLOC_TO_PTR(it));
		free(ALLOC_TO_PTR(it));
	}

	assert(collectable == NULL);

	/* Ok, put all the black nodes back on the collectable list for next
	 * time.
	 */
	if (blackl) {
		printf("Updating collectable\n");
		blackl->prev_collectable = &collectable;
		collectable = blackl;
		blackl = NULL;
	}

	assert(blackl == NULL);
	assert(greyl == NULL);
}
