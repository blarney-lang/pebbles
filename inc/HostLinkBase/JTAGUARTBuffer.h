#ifndef _HOSTLINKBASE_JTAGUARTBUFFER_H_
#define _HOSTLINKBASE_JTAGUARTBUFFER_H_

#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <HostLinkBase/JTAGUART.h>
#include <HostLinkBase/Queue.h>

#define JTAGUART_BUFFER_SIZE 4095

// Buffered I/O over JTAG UART
struct JTAGUARTBuffer {
  JTAGUART* uart;
  Queue<uint8_t>* in;
  Queue<uint8_t>* out;

  JTAGUARTBuffer() {
    uart = new JTAGUART;
    char* str = getenv("PEBBLES_CABLE_ID");
    int cable = str ? atoi(str) : 1;
    uart->open(cable);
    in = new Queue<uint8_t> (JTAGUART_BUFFER_SIZE);
    out = new Queue<uint8_t> (JTAGUART_BUFFER_SIZE);
  }

  // Serve the UART, i.e. fill and release I/O buffers
  void serve() {
    uint8_t buffer[128];
    bool progress = true;
    while (progress) {
      progress = false;
      // Queue -> UART
      if (out->size > 0) {
        int bytes = (unsigned)out->size < sizeof(buffer) ?
                      out->size : sizeof(buffer);
        for (int i = 0; i < bytes; i++) buffer[i] = out->index(i);
        int n = uart->write((char*) buffer, bytes);
        if (n > 0) {
          out->drop(n);
          progress = true;
        }
      }
      // UART -> QUEUE
      if ((unsigned)in->space() > sizeof(buffer)) {
        int n = uart->read((char*) buffer, sizeof(buffer));
        for (int i = 0; i < n; i++) in->enq(buffer[i]);
        if (n > 0) progress = true;
      }
    }
  }

  void flush() {
    while (out->size > 0) {
      serve();
      usleep(100);
    }
  }

  bool canPutBytes(int n) {
    return out->space() >= n;
  };

  void putByte(uint8_t byte) {
    while (out->space() < 1) {
      serve();
      if (out->space() >= 1) break;
      usleep(100);
    }
    out->enq(byte);
  }

  bool canGetBytes(int n) {
    return in->size >= n;
  }

  uint8_t getByte() {
    while (in->size < 1) {
      serve();
      if (in->size >= 1) break;
      usleep(100);
    }
    return in->deq();
  }

  uint8_t peek() {
    return in->first();
  }

  uint8_t peekAt(int i) {
    return in->index(i);
  }

  uint32_t getWord() {
    uint32_t w = getByte();
    w |= getByte() << 8;
    w |= getByte() << 16;
    w |= getByte() << 24;
    return w;
  }

  void putWord(uint32_t w) {
    putByte(w & 0xff); w >>= 8;
    putByte(w & 0xff); w >>= 8;
    putByte(w & 0xff); w >>= 8;
    putByte(w & 0xff);
  }

  ~JTAGUARTBuffer() {
    delete in;
    delete out;
    delete uart;
  }
};

#endif
