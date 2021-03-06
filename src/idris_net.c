// C Library for Idris Network Bindings
// SJF, started on 28/09/13...
#include "idris_net.h"
#define BACKLOG 20

int idrnet_udp_listen(void* conn_info, char* ip, char* port) {
    struct addrinfo hints;
    struct addrinfo* addr_info;
    idr_conn_info* i_conn_info = (idr_conn_info*) conn_info;
    memset(&hints, 0, sizeof(hints));
    // Set up hints to use a datagram socket
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_DGRAM;
    hints.ai_flags = AI_PASSIVE;
    int res;

    // Treat empty IP string as NULL, so IP inferred
    // Will be a Maybe in the Idris code
    if (strlen(ip) == 0) {
        ip = NULL;
        hints.ai_flags = AI_PASSIVE;
    }


    res = getaddrinfo(ip, port, &hints, &addr_info);
    if (res != 0) {
        i_conn_info->last_error = res;
        return res;
    }

    i_conn_info->addr_info = addr_info;

    int sockfd = socket(addr_info->ai_family, addr_info->ai_socktype, addr_info->ai_protocol);
    if (sockfd == -1) {
        i_conn_info->last_error = errno;
        return i_conn_info->last_error;
    }

    res = bind(sockfd, addr_info->ai_addr, addr_info->ai_addrlen);
    if (res == -1) {
        i_conn_info->last_error = errno;
        return i_conn_info->last_error;
    }

    return 0; // Success!
}


void* idrnet_udp_recv(void* conn_info) {
    idr_conn_info* udp_server = (idr_conn_info*) conn_info;
    idr_udp_res* udp_res = (idr_udp_res*) malloc(sizeof(idr_udp_res));
    memset(udp_res, 0, sizeof(idr_udp_res));
    struct sockaddr_storage* remote_host = malloc(sizeof(struct sockaddr_storage));
    int addr_len;

    // TODO: Receive all data available 
    char* data = (char*) malloc(sizeof(char) * BUFFER_SIZE);
    int recv_res = recvfrom(udp_server->sockfd, data, BUFFER_SIZE - 1, 0,
         (struct sockaddr*) remote_host, &addr_len); 

    if (recv_res == -1) {
        udp_res->result = -1;
        udp_res->err = errno;
        return udp_res;
    }

    data[BUFFER_SIZE] = 0x00; // Null-term
    udp_res->result = 0;
    udp_res->err = 0;
    udp_res->fetched_data = data;
    udp_res->remote_host = remote_host;
    udp_res->data_len = recv_res;
    return udp_res;
    
}

void* idrnet_udp_send(char* ip, char* port, char* data) {
    int sockfd;
    struct addrinfo hints;
    struct addrinfo* addr_info;
    int bytes_sent;
    int len = strlen(data);
    idr_udp_send_res* send_res = (idr_udp_send_res*) malloc(sizeof(idr_udp_send_res));
    
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_DGRAM;
    
    int res = getaddrinfo(ip, port, &hints, &addr_info);
    if (res == -1) {
        send_res->result = res;
        send_res->err = errno;
    } 

    sockfd = socket(addr_info->ai_family, addr_info->ai_socktype, addr_info->ai_protocol);

    while (bytes_sent != len) {
        res = sendto(sockfd, data + bytes_sent, len - bytes_sent, 0, addr_info->ai_addr,
                     addr_info->ai_addrlen);

        if (res == -1) {
            send_res->result = -1;
            send_res->err = errno;
        }
        bytes_sent += res;
    }

    close(sockfd);
    return send_res;
     
}

int idrnet_get_udp_send_result(void* udp_send_res) {
    return ((idr_udp_send_res*) udp_send_res)->result;
}

int idrnet_get_udp_send_err(void* udp_send_res) {
    return ((idr_udp_send_res*) udp_send_res)->err;
}

void idrnet_free_udp_send(void* udp_send) {
    free(udp_send);
}
    
void idrnet_free_udp_res(void* udp_res_struct) {
    idr_udp_res* udp_res = (idr_udp_res*) udp_res_struct;
    if (udp_res->fetched_data != NULL) {
        free((char*) (udp_res->fetched_data));
    }

    if (udp_res->remote_host != NULL) {
        free(udp_res->remote_host);
    }
    
    free(udp_res_struct);
}

int idrnet_get_udp_result(void* udp_res_struct) {
    return ((idr_udp_res*) udp_res_struct)->result;
}

int idrnet_get_udp_err(void* udp_res_struct) {
    return ((idr_udp_res*) udp_res_struct)->err;
}

char* idrnet_get_udp_data(void* udp_res_struct) {
    return ((idr_udp_res*) udp_res_struct)->fetched_data;
}

char* idrnet_get_udp_ip(void* udp_res_struct) {
    idr_udp_res* i_res_struct = (idr_udp_res*) udp_res_struct;
    // TODO: this is specific to ipv4, probably want to expand at some stage
    struct sockaddr_in* addr = (struct sockaddr_in*) i_res_struct->remote_host;
    char* printable_addr = malloc(sizeof(char) * INET_ADDRSTRLEN);
    inet_ntop(i_res_struct->remote_host->ss_family, 
      addr->sin_addr, printable_addr, sizeof(printable_addr));
    return printable_addr;
}

int idrnet_get_udp_port(void* udp_res_struct) {
    idr_udp_res* i_res_struct = (idr_udp_res*) udp_res_struct;
    struct sockaddr_in* addr = (struct sockaddr_in*) i_res_struct->remote_host;
    return ((int) (ntohs(addr->sin_port)));
}

int idrnet_listen(void* conn_info, char* ip, char* port) {
    //printf("Listen called\n");
    struct addrinfo* addr_inf;
    struct addrinfo hints;
    idr_conn_info* i_conn_info = (idr_conn_info*) conn_info;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;

    // Treat empty IP string as NULL, so IP inferred
    // Will be a Maybe in the Idris code
    if (strlen(ip) == 0) {
        ip = NULL;
        hints.ai_flags = AI_PASSIVE;
    }

    // Get the address information struct for the given details
    int addr_res = getaddrinfo(ip, port, &hints, &addr_inf);
    if (addr_res != 0) {
        i_conn_info->last_error = addr_res;
        return addr_res;
    }

    // Create a socket
    int sockfd = socket(addr_inf->ai_family, addr_inf->ai_socktype, addr_inf->ai_protocol);
    if (sockfd == -1) {
        i_conn_info->last_error = errno;
        return i_conn_info->last_error;
    }
    i_conn_info->sockfd = sockfd;


    // Allow socket reuse, getting rid of irritating "Address in use" errors
    int y = 1;
    if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &y, sizeof(int)) == -1) {
        i_conn_info->last_error = errno;
        return i_conn_info->last_error;
    }

    // Bind to the socket
    // getaddrinfo does so much awesome stuff for us <3
    int bind_res = bind(sockfd, addr_inf->ai_addr, addr_inf->ai_addrlen);
    if (bind_res == -1) {
        i_conn_info->last_error = errno;
        return i_conn_info->last_error;
    }

    int listen_res = listen(sockfd, BACKLOG); 
    if (listen_res == -1) {
        i_conn_info->last_error = errno;
        return i_conn_info->last_error;
    }

    // Aaaand we're good, return 0 for success
    return 0;
    
}

void* idrnet_accept(void* acceptor_info) {
    idr_conn_info* i_acceptor_info = (idr_conn_info*) acceptor_info;
    struct sockaddr_storage remote_host;
    socklen_t addr_len;
    // Create and nuke result object
    idr_accept_res* i_accept_res = malloc(sizeof(idr_accept_res));
    memset(i_accept_res, 0, sizeof(idr_accept_res));

    int acc_res = accept(i_acceptor_info->sockfd, (struct sockaddr*) &remote_host, &addr_len);
    if (acc_res == -1) {
        i_accept_res->result = -1;
        i_accept_res->err = errno;
        return i_accept_res;
    }

    // Otherwise, acc_res will be our new socket descriptor, so return that
    idr_conn_info* new_conn_info = malloc(sizeof(idr_conn_info));
    memset(new_conn_info, 0, sizeof(idr_conn_info));
    // TODO: Store the address info in addr_info field, not necessary but makes sense
    new_conn_info->sockfd = acc_res;
    i_accept_res->new_client = new_conn_info;
    i_accept_res->err = -1;
    return i_accept_res;
        
}

int idrnet_get_accept_res(void* accept_res_struct) {
    idr_accept_res* i_accept_res = (idr_accept_res*) accept_res_struct;
    return i_accept_res->result;
}

int idrnet_get_accept_err(void* accept_res_struct) {
    idr_accept_res* i_accept_res = (idr_accept_res*) accept_res_struct;
    return i_accept_res->err;
}

void* idrnet_get_accept_client(void* accept_res_struct) {
    idr_accept_res* i_accept_res = (idr_accept_res*) accept_res_struct;
    return i_accept_res->new_client;
}

void idrnet_free_accept_struct(void* accept_res_struct) {
    idr_accept_res* i_accept_res = (idr_accept_res*) accept_res_struct;
    free (i_accept_res);
}


void* idrnet_allocate_conn_info() {
    void* conn_info = malloc(sizeof(idr_conn_info));
    // Nuke it
    memset(conn_info, 0, sizeof(idr_conn_info));
    return conn_info;
}

void idrnet_deallocate_conn_info(void* conn_info) {
    idr_conn_info* i_conn_info = (idr_conn_info*) conn_info;
    if (i_conn_info->addr_info != NULL) {
        freeaddrinfo(i_conn_info->addr_info);
    }

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


int idrnet_connect(void* conn_info, char* ip, char* port) {
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


int idrnet_send(void* conn_info, char* data) {
    int len;
    int bytes_sent;
    idr_conn_info* i_conn_info = (idr_conn_info*) conn_info;

    len = strlen(data);

    // Ensure we send the whole string
    while (bytes_sent != len) {
        int send_res = send(i_conn_info->sockfd, data + bytes_sent, len - bytes_sent, 0);
        if (send_res == -1) {
            i_conn_info->last_error = errno;
            return -1;
        }
        bytes_sent += send_res;
    }
    return bytes_sent;
}

int idrnet_recv(void* conn_info) {
    // BIG TODO: This doesn't receive more data than BUFFER_SIZE! This should be rectified.
    char* data = (char*) malloc(sizeof(char) * BUFFER_SIZE);
    idr_conn_info* i_conn_info = (idr_conn_info*) conn_info;
    // Firstly, nuke any of the data that might still be in there...
//    printf("Recv called, conn_info: %p\n", conn_info);

    if (i_conn_info->fetched_data != NULL) {
        free((void*)(i_conn_info->fetched_data));
    }

    // We need to make sure we null terminate
    int num_bytes = recv(i_conn_info->sockfd, data, BUFFER_SIZE - 1, 0);
    //printf("C: num_bytes: %i\n", num_bytes);
    if (num_bytes < 0) {
        // Error reading from socket
        i_conn_info->last_error = errno;
        free(data);
//        printf("C: Error reading from socket: %i\n", errno);
        return errno;
    } else if (num_bytes == 0) {
        // Connection closed
        i_conn_info->last_error = 0;
        free(data);
//        printf("C: Socket closed\n");
        return 0;
    }
    data[num_bytes] = '\0';
    i_conn_info->fetched_data = data;
    return num_bytes;
}

// Just to allay any potential concurrency issues.
// We pass around the state for every connection -- so why not just store the last error in that?
int idrnet_get_last_error(void* conn_info) {
    idr_conn_info* i_conn_info = (idr_conn_info*) conn_info;
    return i_conn_info->last_error;
}

char* idrnet_get_fetched_data(void* conn_info) {
    idr_conn_info* i_conn_info = (idr_conn_info*) conn_info;
//    printf("C: Fetched data %s\n", i_conn_info->fetched_data);
    return i_conn_info->fetched_data;

}

