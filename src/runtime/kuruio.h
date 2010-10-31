typedef struct kuru_string_t {
   int length;
	char *data;
} kuru_string_t;

void io_init(void);
int  io_puts(const kuru_string_t message);
void io_done(void);
