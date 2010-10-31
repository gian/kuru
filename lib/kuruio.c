#include "kuruio.h"
#include "channels.h"
#include "pthread.h"
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

channel_t *iochan;
pthread_t iothread;

typedef struct io_message_t {
	enum { IO_PUTS, IO_DONE } type;
	void *data;
} io_message_t;

static bool io_handle_message(io_message_t *message)
{
	switch(message->type) {
		case IO_PUTS:
			puts(message->data);
			free(message->data);
			return true;
		case IO_DONE:
			return false;
	}
	assert(!"Unknown IO Message");
	return false;
}

static void *io_thread(void *dummy)
{
	bool running;
	(void)dummy; /* force usage */
	do {
		io_message_t *msg = channel_get(iochan);
		printf("Message %d\n",msg->type);
		running = io_handle_message(msg);
		free(msg);
	} while(running);

	return NULL;
}

void io_init(void)
{
	iochan = channel_create();
	pthread_create(&iothread, NULL, io_thread, NULL);
}

void io_puts(const char *message)
{
	io_message_t *msg = malloc(sizeof(io_message_t));
	msg->type = IO_PUTS;
	msg->data = strdup(message);
	channel_put(iochan, msg);
}

void io_done(void)
{
	io_message_t *msg = malloc(sizeof(io_message_t));
	msg->type = IO_DONE;
	msg->data = NULL;
	channel_put(iochan, msg);
	pthread_join(iothread,NULL);
	channel_destroy(iochan);
}
