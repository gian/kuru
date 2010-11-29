#include "kuru_types.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>

kuru_string_t *c2ks(const char *cstr)
{
	printf("Duping (%p) %s\n", cstr, cstr);
	kuru_string_t *ret = malloc(sizeof(kuru_string_t));
	ret->data = strdup(cstr);
	ret->length = strlen(ret->data);
	return ret;
}

void kuru_string_destroy(kuru_string_t *kstr)
{
	free(kstr->data);
	free(kstr);
}

kuru_string_t *ksdup(const kuru_string_t *kstr)
{
	kuru_string_t *ret = malloc(sizeof(kuru_string_t));
	ret->length = kstr->length;
	ret->data = malloc(ret->length);
	memcpy(ret->data, kstr->data, ret->length);
	return ret;
}
