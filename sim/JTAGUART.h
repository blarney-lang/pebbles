// Simulate JTAG UART using UNIX domain socket

#ifndef _JTAGUART_H_
#define _JTAGUART_H_

#include <SoC.h>
#include <queue>
#include <stdint.h>
#include <assert.h>
#include <Socket.h>

// Parameters
// ==========

// Default socket name to bind to
#define JTAGUART_SOCKET_NAME "jtaguart"

// Max size of internal queues
#define JTAGUART_WQUEUE_SIZE 64
#define JTAGUART_RQUEUE_SIZE 64

// Types
// =====

// Avalon MM interface to JTAGUART
struct JTAGUARTInterface {
  // Signals from the JTAGUART
  uint32_t* readdata;
  uint8_t* waitrequest;

  // Signals to the JTAGUART
  uint32_t* writedata;
  uint8_t* address;
  uint8_t* read;
  uint8_t* write;
};

// JTAGUART simulator
// ==================

struct JTAGUART {
  // Avalon interface signals
  JTAGUARTInterface ifc;

  // Listening socket
  int sock;

  // Connected socket
  int conn;

  // Read/write queues
  std::queue<uint8_t> writeQueue;
  std::queue<uint32_t> readQueue;

  // Read latch
  uint32_t readLatch;

  // Constructor
  JTAGUART() {
    // Ignore SIGPIPE
    signal(SIGPIPE, SIG_IGN);

    // Initialise sockets
    sock = socketListen(JTAGUART_SOCKET_NAME);
    conn = -1;
  }

  // Destructor
  ~JTAGUART() {
    close(sock);
  }

  void tick() {
    // Can't read and write at same time
    assert(!(*ifc.read && *ifc.write));

    // Assign output signals
    *ifc.waitrequest = 0;
    *ifc.readdata = readLatch;

    // Handle write
    if (*ifc.write) {
      assert(*ifc.address == 0);
      writeQueue.push(*ifc.writedata);
      // This assertion will catch the case where the user writes
      // even though there is no space.  We could simply drop the
      // write in this case, but in simulation, it's better to
      // indicate something's gone wrong.
      assert(writeQueue.size() <= JTAGUART_WQUEUE_SIZE);
    }

    // Handle read
    if (*ifc.read) {
      if (*ifc.address == 0) {
        // Data read
        if (readQueue.size() > 0) {
          readLatch = 0x8000 | readQueue.front();
          readQueue.pop();
        }
        else {
          readLatch = 0;
        }
      }
      else if (*ifc.address == 4) {
        // CSR read
        uint32_t wspace = writeQueue.size() < JTAGUART_WQUEUE_SIZE;
        readLatch = wspace << 16;
      }
    }

    // Try to write byte to socket
    if (writeQueue.size() > 0) {
      if (socketPutByte(sock, &conn, writeQueue.front()) > 0)
        writeQueue.pop();
    }

    // Try to read byte from socket
    if (readQueue.size() < JTAGUART_RQUEUE_SIZE) {
      int ret = socketGetByte(sock, &conn);
      if (ret >= 0)
        readQueue.push(ret);
    }
  }
};

#endif
