// C Library for Idris Network Bindings
// SJF, started on 28/09/13...
#include "idris_net.h"

void* idrnet_listen(VM* vm, const char* ip, const char* port) {
}

void* idrnet_allocate_conn_info() {
    void* conn_info = malloc(sizeof(idr_conn_info));
    memset(conn_info, 0, sizeof(idr_conn_info));
    return conn_info;
}

void idrnet_deallocate_conn_info(void* conn_info) {
    free(conn_info);
}

int idrnet_close(void* conn_info) {
    idr_conn_info* i_conn_info = (idr_conn_info*) conn_info;
    int close_res = close(i_conn_info->sockfd);
    if (close_res != 0) {
        i_conn_info->last_error = errno;
        return errno;
    }

    return 0;
}


int idrnet_connect(void* conn_info, const char* ip, const char* port) {
    struct addrinfo* addr_inf;
    struct addrinfo hints;
    
    idr_conn_info* i_conn_info = (idr_conn_info*) conn_info;
    // Zero out the hints struct
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET; // Much to the disapproval of profmakx, IPV4 right now
    hints.ai_socktype = SOCK_STREAM; // TCP

    int status = getaddrinfo(ip, port, &hints, &addr_inf);
    if (status != 0) { // Error occurred...
        i_conn_info->last_error = status;
        return status;
    } 
    
    // Get a socket descriptor we may use for requests
    int socketfd = socket(addr_inf->ai_family, addr_inf->ai_socktype, addr_inf->ai_protocol);
    if (socketfd == -1) {
        i_conn_info->last_error = errno;
        return errno;
    }

    // Perform the connection
    int conn_res = connect(socketfd, addr_inf->ai_addr, addr_inf->ai_addrlen);
    if (conn_res == -1) {
        i_conn_info->last_error = errno;
        return errno;
    }

    // If all's good, add information to the connection state and return 0 for success.
    i_conn_info->addr_info = addr_inf;
    i_conn_info->sockfd = socketfd;

    return 0;
}


int idrnet_send(void* conn_info, const char* data) {
    int len;
    int bytes_sent;
    idr_conn_info* i_conn_info = (idr_conn_info*) conn_info;

    len = strlen(data);

    // Ensure we send the whole string
    while (bytes_sent != orig_len) {
        int send_res = send(i_conn_info->sockfd, data + bytes_sent, len - bytes_sent, 0);
        if (send_res == -1) {
            i_conn_info->last_error = errno;
            return -1;
        }
        bytes_sent += send_res;
    }
    return bytes_sent;
}

const char* idrnet_recv(void* conn_info) {
    // BIG TODO: This doesn't receive more data than BUFFER_SIZE! This should be rectified.
    char* data = (char*) malloc(sizeof(char) * BUFFER_SIZE);
    idr_conn_info i_conn_info = (idr_conn_info*) conn_info;
    // We need to make sure we null terminate
    int num_bytes = recv(i_conn_info->sockfd, data, BUFFER_SIZE - 1, 0);
    if (num_bytes < 0) {
        // Error reading from socket
        i_conn_info->last_error = errno;
        return NULL;
    } else if (num_bytes == 0) {
        // Connection closed
        i_conn_info->last_error = 0;
        return NULL;
    }
    data[num_bytes] = '\0';
    return data;
}

// Just to allay any potential concurrency issues.
// We pass around the state for every connection -- so why not just store the last error in that?
int idrnet_get_last_error(void* conn_info) {
    idr_conn_info* i_conn_info = (idr_conn_info*) conn_info;
    return conn_info->last_error;
}
