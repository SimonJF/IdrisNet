// C Library for Idris Network Bindings
// SJF, started on 28/09/13...
#ifndef IDRISNET_H
#define IDRISNET_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <idris_rts.h>
#include <errno.h>

// Raw structure, passed to Idris as a void* pointer.
// Contains connection-specific state, used in operations such as send and receive.
typedef struct idr_conn_info {
    struct addrinfo* addr_info;
    int sockfd;
} idr_conn_info;

// API:

// Listens on a TCP socket with the given IP and port.
// Idris will have checked IP and port at this point, and serialised to strings.
// TODO: Perhaps add other params in
// Returns some network result, which will be a native Idris type.
void* idrnet_listen(VM* vm, const char* ip, const char* port); 

// Attempts to connect to a TCP socket with the given IP and port.
// Again, Idris will have checked IP and port by this point.
void* idrnet_connect(VM* vm, const char* ip, const char* port);

// Sends data to the given connection.
// (We assume flags are 0 atm.)
void* idrnet_send(VM* vm, void* conn_info, const char* data);

// Receives data from the given connection.
// Buffer length, I know...
void* idrnet_recv(VM* vm, void* conn_info);

// Internal:


#endif
