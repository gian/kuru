#include <pthread.h>

typedef struct channel_t {
	pthread_mutex_t mutex;
	pthread_cond_t value_ready;
	pthread_cond_t value_empty;
	void *value;
} channel_t;

channel_t *channel_create(void);
void channel_put(channel_t *channel, void *value);
void *channel_get(channel_t *channel);
void channel_destroy(channel_t *channel);

