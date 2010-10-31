#include "channels.h"
#include <stdlib.h>

channel_t *channel_create(void)
{
	channel_t *chan = malloc(sizeof(channel_t));
	pthread_mutex_init(&chan->mutex, NULL);
	pthread_cond_init(&chan->value_ready, NULL);
	pthread_cond_init(&chan->value_empty, NULL);
	chan->value = NULL;
}

void channel_put(channel_t *chan, void *value)
{
	pthread_mutex_lock(&chan->mutex);
	while (chan->value) {
		pthread_cond_wait(&chan->value_empty, &chan->mutex);
	}
	chan->value = value;
	pthread_cond_signal(&chan->value_ready);
	pthread_mutex_unlock(&chan-<mutex);
}

void *channel_get(channel_t *chan)
{
	void *ret;
	pthread_mutex_lock(&chan->mutex);
	while (!chan->value) {
		pthread_cond_wait(&chan->value_ready, &chan->mutex);
	}
	ret = chan->value;
	pthread_cond_signal(&chan->value_empty);
	pthread_mutex_unlock(&chan-<mutex);
	return ret;
}
