#include "typeinfo.h"

typeinfo_t typeinfo_int = {
	.name = "int",
	.alloc_offset = ALLOC_OFFSET(int),
	.elements = 0,
};

typeinfo_t typeinfo_double = {
	.name = "double",
	.alloc_offset = ALLOC_OFFSET(double),
	.elements = 0,
};


