// Non-blocking connection to Altera's JTAG UART.  This class
// abstracts over whether the SoC is running in simulation or on FPGA.

#ifndef _HOSTLINK_JTAGUART_H_
#define _HOSTLINK_JTAGUART_H_

#ifdef SIMULATE

// =============================================================================
// Simulation version
// =============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <poll.h>
#include <assert.h>
#include <HostLink/Socket.h>

class JTAGUART {
  int sock;

 public:

  // Constructor
  JTAGUART() { sock = -1; }

  // Open UART with given id
  void open(int id) {
    char sockName[1024];
    snprintf(sockName, sizeof(sockName), "jtaguart%d", id);
    sock = socketConnect(sockName);
    if (sock < 0) {
      fprintf(stderr, "Please check that the simulator is running\n");
      exit(EXIT_FAILURE);
    }
  }

  // Send bytes over UART
  int write(char* data, int numBytes) {
    int ret = send(sock, (void*) data, numBytes, 0);
    if (ret < 0) {
      if (errno == EAGAIN || errno == EWOULDBLOCK) return 0;
    }
    return ret;
  }

  // Receive bytes over UART
  int read(char* data, int numBytes) {
    int ret = recv(sock, (void*) data, numBytes, 0);
    if (ret < 0) {
      if (errno == EAGAIN || errno == EWOULDBLOCK) return 0;
    }
    return ret;
  }

  // Flush writes
  void flush() {
    if (sock != -1) fsync(sock);
  }

  // Close UART
  void close() {
    if (sock != -1) {
      ::close(sock);
      sock = -1;
    }
  }

  // Destructor
  ~JTAGUART() {
    close();
  }
};

#else

// =============================================================================
// FPGA version
// =============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <HostLink/JTAGAtlantic.h>

class JTAGUART {
  JTAGATLANTIC* jtag;

 public:

  // Constructor
  JTAGUART() { jtag = NULL; }

  // Open UART with given id
  void open(int id) {
    char chain[1024];
    snprintf(chain, sizeof(chain), "%i", id);
    jtag = jtagatlantic_open(chain, 0, 0, "HostLink");
    if (jtag == NULL) {
      fprintf(stderr, "Error opening JTAG UART %i\n", id);
      exit(EXIT_FAILURE);
    }
  }

  // Send bytes over UART
  int write(char* data, int numBytes) {
    assert(jtag != NULL);
    return jtagatlantic_write(jtag, (char*) data, numBytes);
  }

  // Receive bytes over UART
  int read(char* data, int numBytes) {
    assert(jtag != NULL);
    return jtagatlantic_read(jtag, (char*) data, numBytes);
  }

  // Flush writes
  void flush() {
    if (jtag != NULL) jtagatlantic_flush(jtag);
  }

  // Close UART
  void close() {
    if (jtag != NULL) {
      jtagatlantic_close(jtag);
      jtag = NULL;
    }
  }

  // Destructor
  ~JTAGUART() {
    close();
  }
};

#endif

#endif
