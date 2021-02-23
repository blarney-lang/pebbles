#ifndef _DRAM_H_
#define _DRAM_H_

#include <queue>
#include <cstring>
#include <stdint.h>
#include <assert.h>
#include <SoC.h>

// Parameters
// ==========

// DRAM contents is split into fixed-size pages, allocated on demand
#define LogBeatsPerPage 14

// DRAMLatency on load responses (ticks)
#define DRAMLatency 20

// Types
// =====

// Avalon MM interface to DRAM
struct DRAMInterface {
  // Signals from DRAM
  uint32_t* readdata;
  uint8_t* waitrequest;
  uint8_t* readdatavalid;

  // Signals to DRAM
  uint32_t* writedata;
  uint64_t address;
  uint64_t burstcount;
  uint64_t byteen;
  bool read;
  bool write;
};

// DRAM response
struct DRAMResponse {
  bool valid;
  uint32_t beat[DRAMBeatWords];
};

// Functions
// =========

// Extract byte from word
inline uint8_t getByte(uint32_t x, uint32_t idx) {
  x >>= idx*8;
  return x & 0xff;
}

// Constrct word from bytes
inline uint32_t makeWord(uint8_t* ptr)
{
  uint32_t ret = ptr[0];
  ret |= (ptr[1] << 8);
  ret |= (ptr[2] << 16);
  ret |= (ptr[3] << 24);
  return ret;
}

// DRAM simulator
// ==============

struct DRAM {
  // Avalon interface signals
  DRAMInterface ifc;

  // Contents represented as array of pages
  uint64_t numPages;
  uint8_t** pages;

  // Internal state
  uint64_t writeBurstCount;
  uint64_t writeBurstLen;
  uint64_t writeBurstAddr;
  uint64_t readBurstCount;
  uint64_t readBurstLen;
  uint64_t readBurstAddr;

  // Response queue (to simulate latency)
  std::queue<DRAMResponse> respQueue;

  // Constructor
  DRAM() {
    // Allocate array of pages
    assert(DRAMAddrWidth >= LogBeatsPerPage);
    numPages = 1 << (DRAMAddrWidth - LogBeatsPerPage);
    pages = new uint8_t* [numPages];
    for (uint64_t i = 0; i < numPages; i++) pages[i] = NULL;

    // Initialisation
    writeBurstLen = writeBurstCount = 1;
    readBurstLen = readBurstCount = 1;

    // Introduce latency
    for (uint32_t i = 0; i < DRAMLatency; i++) {
      DRAMResponse resp;
      resp.valid = false;
      respQueue.push(resp);
    }
  }

  // Destructor
  ~DRAM() {
    for (uint64_t i = 0; i < numPages; i++)
      if (pages[i] != NULL) delete [] pages[i];
  }

  void tick() {
    // Can't read and write at same time
    assert(!(ifc.read && ifc.write));

    // Read should never happen during write burst
    assert(ifc.read ? writeBurstCount == 1 : true);

    // Response queue will never be empty (assuming DRAMLatency > 1)
    assert(respQueue.size() > 0);

    // Assign output signals
    DRAMResponse resp = respQueue.front();
    *ifc.readdatavalid = resp.valid;
    for (uint32_t i = 0; i < DRAMBeatWords; i++)
      ifc.readdata[i] = resp.beat[i];
    respQueue.pop();

    // Handle write
    if (ifc.write) {
      // Burst count must be non-zero
      assert(ifc.burstcount > 0);
      // Save burst length and address on first beat of burst
      if (writeBurstCount == 1) {
        writeBurstLen = ifc.burstcount;
        writeBurstAddr = ifc.address;
      }
      // Deconstruct address
      uint64_t addr = writeBurstAddr + (writeBurstCount-1);
      uint64_t page = addr >> LogBeatsPerPage;
      uint64_t beatOffset = addr & ((1 << LogBeatsPerPage) - 1);
      uint64_t byteOffset = beatOffset * DRAMBeatBytes;
      assert(page < numPages);
      // Allocate page if it doesn't exist
      if (pages[page] == NULL) {
        uint32_t pageLen = (1 << LogBeatsPerPage) * DRAMBeatBytes;
        pages[page] = new uint8_t [pageLen];
        memset(pages[page], 0, pageLen);
      }
      // Write data
      for (uint32_t i = 0; i < DRAMBeatBytes; i++) {
        if (ifc.byteen & (1ul << i))
          pages[page][byteOffset+i] =
            getByte(ifc.writedata[i/4], (i%4));
      }
      // Increment burst count, and reset on final beat of burst
      if (writeBurstCount == writeBurstLen)
        writeBurstCount = 1;
      else
        writeBurstCount++;
    }

    // Handle read
    if (ifc.read || readBurstCount > 1) {
      // Burst count must be non-zero on a read
      assert(ifc.read ? ifc.burstcount > 0 : true);
      // Save burst length and address on first beat of burst
      if (readBurstCount == 1) {
        readBurstLen = ifc.burstcount;
        readBurstAddr = ifc.address;
      }
      // Deconstruct address
      uint64_t addr = readBurstAddr + (readBurstCount-1);
      uint64_t page = addr >> LogBeatsPerPage;
      uint64_t beatOffset = addr & ((1 << LogBeatsPerPage) - 1);
      uint64_t byteOffset = beatOffset * DRAMBeatBytes;
      assert(page < numPages);
      // Does page exist?
      if (pages[page] == NULL) {
        resp.valid = true;
        for (uint32_t i = 0; i < DRAMBeatWords; i++)
          resp.beat[i] = 0;
        respQueue.push(resp);        
      }
      else {
        resp.valid = true;
        for (uint32_t i = 0; i < DRAMBeatWords; i++) {
          resp.beat[i] = makeWord(&pages[page][byteOffset+4*i]);
        }
        respQueue.push(resp);
      }
      // Increment burst count, and reset on final beat of burst
      if (readBurstCount == readBurstLen)
        readBurstCount = 1;
      else
        readBurstCount++;
    }
    else {
      // Inject invalid response (a response must be injected on every tick)
      resp.valid = false;
      respQueue.push(resp);
    }

    // Assert wait request during burst read
    *ifc.waitrequest = readBurstCount > 1;
  }
};

#endif
