#ifndef _HOSTLINK_H_
#define _HOSTLINK_H_

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <SoC.h>
#include <HostLink/JTAGUARTBuffer.h>
#include <HostLink/MemFileReader.h>

class HostLink {

 public:

  // Access to SoC via UART
  JTAGUARTBuffer* uart;

  // Constructor
  HostLink() {
    uart = new JTAGUARTBuffer;
  }

  // Destructor
  ~HostLink() {
    delete uart;
  }

  // Assuming the boot loader is running on the CPU
  // ----------------------------------------------

  // Load application code and data onto the SoC
  void boot(const char* codeFilename, const char* dataFilename) {
    MemFileReader code(codeFilename);
    MemFileReader data(dataFilename);

    // Send code
    uint32_t addr, word;
    while (code.getWord(&addr, &word)) {
      uart->putWord(addr);
      uart->putWord(word);
    }
    uart->putWord(0xffffffff);  // Terminator

    // Send data
    while (data.getWord(&addr, &word)) {
      uart->putWord(addr);
      uart->putWord(word);
    }
    uart->putWord(0xffffffff);  // Terminator
    uart->flush();
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
