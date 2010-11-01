#include "channels.h"
#include <stdlib.h>
#include <assert.h>

channel_t *channel_create(void)
{
	channel_t *chan = malloc(sizeof(channel_t));
	pthread_mutex_init(&chan->mutex, NULL);
	pthread_cond_init(&chan->value_ready, NULL);
	pthread_cond_init(&chan->value_empty, NULL);
	chan->value = NULL;
	return chan;
}

void channel_put(channel_t *chan, void *value)
{
	assert(value && "You cannot put NULL onto a channel");
	assert(chan && "You cannot put onto a NULL channel");
	pthread_mutex_lock(&chan->mutex);
	while (chan->value) {
		pthread_cond_wait(&chan->value_empty, &chan->mutex);
	}
	chan->value = value;
	pthread_cond_signal(&chan->value_ready);
	pthread_mutex_unlock(&chan->mutex);
}

void *channel_get(channel_t *chan)
{
	void *ret;
	assert(chan && "Cannot get on a NULL channel");
	pthread_mutex_lock(&chan->mutex);
	while (!chan->value) {
		pthread_cond_wait(&chan->value_ready, &chan->mutex);
	}
	ret = chan->value;
	chan->value = NULL; 
	pthread_cond_signal(&chan->value_empty);
	pthread_mutex_unlock(&chan->mutex);
	return ret;
}

void channel_destroy(channel_t *chan)
{
	assert(chan && "Cannot destroy a NULL channel");
	pthread_mutex_lock(&chan->mutex);
	assert(!chan->value);
	pthread_mutex_destroy(&chan->mutex);
	pthread_cond_destroy(&chan->value_ready);
	pthread_cond_destroy(&chan->value_empty);
	free(chan);
}
