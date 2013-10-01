// C Library for Idris Network Bindings
// SJF, started on 28/09/13...
#ifndef IDRISNET_H
#define IDRISNET_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <idris_rts.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

#define BUFFER_SIZE 1024

// Raw structure, passed to Idris as a void* pointer.
// Contains connection-specific state, used in operations such as send and receive.
typedef struct idr_conn_info {
    struct addrinfo* addr_info;
    int sockfd;
    int last_error;
    const char* fetched_data;
} idr_conn_info;

// API:

// Listens on a TCP socket with the given IP and port.
// Idris will have checked IP and port at this point, and serialised to strings.
// TODO: Perhaps add other params in
void* idrnet_listen(const char* ip, const char* port); 

// Attempts to connect to a TCP socket with the given IP and port.
// Again, Idris will have checked IP and port by this point.
int idrnet_connect(void* conn_info, const char* ip, const char* port);

// Attempts to close the current connection
int idrnet_close(void* conn_info);


// Sends data to the given connection.
// (We assume flags are 0 atm.)
// Returns -1 on error, or number of bytes sent otherwise
int idrnet_send(void* conn_info, const char* data);

// Receives data from the given connection.
// Returns NULL if there's a 
int idrnet_recv(void* conn_info);

// Allocates an idr_conn_info struct, which is passed into subsequent calls
void* idrnet_allocate_conn_info();

// Frees the idr_conn_info struct
void idrnet_deallocate_conn_info(void* conn_info);

// Returns the last error stored in the connection state
int idrnet_get_last_error();

// Retrieve result of last fetch (as we can't do a null check on a string in idris...)
const char* idrnet_get_fetched_data();

#endif
