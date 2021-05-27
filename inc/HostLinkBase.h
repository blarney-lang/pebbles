#ifndef _HOSTLINKBASE_H_
#define _HOSTLINKBASE_H_

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <Config.h>
#include <HostLinkBase/JTAGUARTBuffer.h>
#include <HostLinkBase/MemFileReader.h>

#ifdef SIMULATE
#define IsSimulation 1
#else
#define IsSimulation 0
#endif

class HostLinkBase {

 public:

  // Access to SoC via UART
  JTAGUARTBuffer* uart;

  // Constructor
  HostLinkBase() {
    uart = new JTAGUARTBuffer;
  }

  // Destructor
  ~HostLinkBase() {
    delete uart;
  }

  // Dumping data from the UART
  // --------------------------

  // Receive bytes from SoC and display on stdout
  // Returns when an '\0' is observed
  void dump() {
     while (1) {
       uint8_t b = uart->getByte();
       if (b == '\0') break;
       printf("%c", b);
     }
  }
};

#endif
