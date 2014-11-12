/*
 * egg unix domain socket connetion helper.
 * This program is based on icanna.c (yc-el's canna unix domain socket helper)
 *
 * Copyright (c) 2005 ISHIKAWA Mutsumi
 *
*/

/* icanna.c
 * VERSION: 0.9.0
 * AUTHER: knak@ceres.dti.ne.jp
 * DATE: 2003.9.29
 * LICENCE: GPL
 */

/*
 * communicate unix domain IM server
 * stdin -> IM server -> stdout
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

#define BFSZ (4096)  /* buffer size */

#define SUNMAX 108   /* sockaddr_un.sun_path length = 108 */

/*
 * connect unix domain IM server
 */
int connect_server(const char *sock_path)
{
	int sockfd;
	struct sockaddr_un sun;
	
	/* create unix domain socket */
	if ((sockfd = socket(PF_UNIX, SOCK_STREAM, 0)) < 0) {
		perror("unable open socket");
		exit(1);
	}

	/* connect IM server */
	sun.sun_family = AF_UNIX;
	strncpy(sun.sun_path, sock_path, SUNMAX - 1);
	if (connect(sockfd, (struct sockaddr*)&sun, SUN_LEN(&sun)) < 0) {
		perror("unable connect");
		exit(1);
	}
	return sockfd;
}

/*
 * data transport
 * stdin -> IM server
 * IMserver -> stdout
 */
int transport(int in, int out)
{
	char* buf = NULL; /* data buffer */
	int len = BFSZ;	  /* data length */
	int count = -1;	  /* read count */
	
	/* read input */
	while (len == BFSZ) {
		count++;
		/* allocate data buffer */
		if ((buf = (char*)realloc(buf, (count + 1) * BFSZ)) == NULL) {
			perror("realloc");
			exit(1);
		}
		/* read input to data buffer */
		if ((len = read(in, buf + count * BFSZ, BFSZ)) < 0) {
			perror("read");
			exit(1);
		}
	}
	len += count * BFSZ;
	/* write output */
	if (len > 0 && write(out, buf, len) < 0) {
		perror("write");
		exit(1);
	}
	/* destroy data buffer */
	free(buf);
	return len;
}

/*
 * communicate unix domain IM server
 */
int main(int argc, char *argv[])
{
	int server;
	struct stat stat_buf;
	
	if ( argc < 2 ) {
		fprintf(stderr, "usage: egg-helper socket_path\n");
		exit(1);
	}

	/* connect IM server via unix domain socket */
	server = connect_server(argv[1]);
	
	/* transport request & response, until stdin or IM server is eof */
	while (1)
		/* transport request from stdin to IM server,
		 * transport response from IM server to stdout */
		if (transport(0, server) == 0 || transport(server, 1) == 0)
			/* when stdin or IM server is eof, break loop */
			break;
	close(server); /* close IM server via unix domain socket */
	exit(0);
}
