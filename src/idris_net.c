// C Library for Idris Network Bindings
// SJF, started on 28/09/13...
#include "idris_net.h"

// If I'm understanding correctly...
// idris_constructor: Closure, VM, Tag, Arity, Outer lock

#define CONN_SUCCESS_TAG 0
#define CONN_FAIL_TAG 1

void* idrnet_listen(VM* vm, const char* ip, const char* port) {
}

typedef enum ConnFailReason { ADDR, SOCKET, CONNECT } ConnFailReason;

VAL connFail(VM* vm, ConnFailReason reason, int error_code) {
    VAL err;
    idris_constructor(err, vm, 0, 1, 0);
    idris_setConArg(err, 0, MKINT((intptr_t) error_code)); // Not sure whether we need the cast

    VAL res;
    idris_constructor(res, vm, CONN_FAIL_TAG, 1, 0);
    idris_setConArg(res, 0, err);
}

void* idrnet_connect(VM* vm, const char* ip, const char* port) {
    struct addrinfo* addr_inf;
    struct addrinfo hints;
    
    // Zero out the hints struct
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET; // Much to the disapproval of profmakx, IPV4 right now
    hints.ai_socktype = SOCK_STREAM; // TCP

    int status = getaddrinfo(ip, port, &hints, &addr_inf);
    if (status != 0) { // Error occurred...
        return connFail(vm, ADDR, status);
    } 
    
    // Get a socket descriptor we may use for requests
    int socketfd = socket(addr_inf->ai_family, addr_inf->ai_socktype, addr_inf->ai_protocol);
    if (socketfd == -1) {
        return connFail(vm, SOCKET, errno);
    }

    // Perform the connection
    int conn_res = connect(socketfd, addr_inf->ai_addr, addr_inf->ai_addrlen);
    if (conn_res == -1) {
        return connFail(vm, CONNECT, errno);
    }

    // If all's good, make a connection state, and pass back up to the Idris code
    idr_conn_info* conn_info = (idr_conn_info*) malloc(sizeof(idr_conn_info));
    conn_info->addr_info = addr_inf;
    conn_info->sockfd = socketfd;

    VAL res;
    idris_constructor(res, vm, CONN_SUCCESS_TAG, 1, 0);
    idris_setConArg(res, 0, (void*) conn_info);
    return res;
}


void* idrnet_send(VM* vm, void* conn_info, const char* data) {
}

void* idrnet_recv(VM* vm, void* conn_info) {

}
