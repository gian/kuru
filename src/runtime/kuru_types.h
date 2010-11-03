typedef struct kuru_string_t {
   int length;
  char *data;
} kuru_string_t;

typedef struct kuru_list_t {
	char *data;
	struct kuru_list_t *next;
} kuru_list_t;

kuru_string_t *c2ks(const char *cstr);
void kuru_string_destroy(kuru_string_t *kstr);
kuru_string_t *ksdup(const kuru_string_t *kstr);
