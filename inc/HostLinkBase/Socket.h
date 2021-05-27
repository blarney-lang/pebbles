#ifndef _HOSTLINKBASE_SOCKET_H_
#define _HOSTLINKBASE_SOCKET_H_

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <ctype.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <assert.h>
#include <signal.h>

// Make a socket non-blocking
inline void socketSetNonBlocking(int sock)
{
  int flags = fcntl(sock, F_GETFL, 0);
  if (flags == -1) {
    perror("fcntl");
    exit(EXIT_FAILURE);
  }
  int ret = fcntl(sock, F_SETFL, flags|O_NONBLOCK);
  if (ret == -1) {
    perror("fcntl");
    exit(EXIT_FAILURE);
  }
}

// Create listening socket
inline int socketListen(const char* socketName)
{
  // Create socket
  int sock = socket(AF_UNIX, SOCK_STREAM, 0);
  if (sock == -1) {
    perror("JTAGUART simulator: socket");
    exit(EXIT_FAILURE);
  }

  // Bind socket
  struct sockaddr_un sockAddr;
  memset(&sockAddr, 0, sizeof(struct sockaddr_un));
  sockAddr.sun_family = AF_UNIX;
  sockAddr.sun_path[0] = '\0';
  strncpy(&sockAddr.sun_path[1], socketName,
    sizeof(sockAddr.sun_path)-2);
  int ret = bind(sock, (const struct sockaddr *) &sockAddr,
                   sizeof(struct sockaddr_un));
  if (ret == -1) {
    perror("JTAGUART simulator: bind");
    exit(EXIT_FAILURE);
  }

  // Listen for connections
  ret = listen(sock, 0);
  if (ret == -1) {
    perror("JTAGUART simulator: listen");
    exit(EXIT_FAILURE);
  }

  // Make it non-blocking
  socketSetNonBlocking(sock);

  return sock;
}

// Accept connection
inline void socketAccept(int sock, int* conn)
{
  assert(sock >= 0);

  if (*conn != -1) return;

  // Accept connection
  *conn = accept(sock, NULL, NULL);

  // Make connection non-blocking
  if (*conn != -1)
    socketSetNonBlocking(*conn);
}

// Non-blocking read
int socketGetByte(int sock, int* conn)
{
  uint8_t byte;
  socketAccept(sock, conn);
  if (*conn == -1) return -1;
  int n = read(*conn, &byte, 1);
  if (n == 1)
    return (int) byte;
  else if (!(n == -1 && errno == EAGAIN)) {
    close(*conn);
    *conn = -1;
  }
  return -1;
}

// Non-blocking write of 8 bits
int socketPutByte(int sock, int* conn, uint8_t byte)
{
  socketAccept(sock, conn);
  if (*conn == -1) return 0;
  int n = write(*conn, &byte, 1);
  if (n == 1)
    return 1;
  else if (!(n == -1 && errno == EAGAIN)) {
    close(*conn);
    *conn = -1;
  }
  return 0;
}

// Open UART socket
int socketConnect(const char* socketName)
{
  // Create socket
  int sock = socket(AF_UNIX, SOCK_STREAM, 0);
  if (sock == -1) {
    perror("socket");
    return -1;
  }

  // Connect to socket
  struct sockaddr_un addr;
  memset(&addr, 0, sizeof(struct sockaddr_un));
  addr.sun_family = AF_UNIX;
  snprintf(&addr.sun_path[1], sizeof(addr.sun_path)-2, "%s", socketName);
  addr.sun_path[0] = '\0';
  int ret = connect(sock, (struct sockaddr *) &addr,
              sizeof(struct sockaddr_un));
  if (ret < 0) {
    perror("connect");
    return -1;
  }

  // Make it non-blocking
  socketSetNonBlocking(sock);

  return sock;
}

#endif
