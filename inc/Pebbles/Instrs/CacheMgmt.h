#ifndef _PEBBLES_CACHEMGMT_H_
#define _PEBBLES_CACHEMGMT_H_

#include <Pebbles/Common.h>
#include <Pebbles/Instrs/Fence.h>

// Cache line flush
INLINE void pebblesCacheFlushLine(void* addr)
{
  // Custom instruction
  // Opcode: 0000000 rs2 rs1 000 rd 0001000, with rd=0, rs1=x10, rs2=0
  asm volatile(
    "mv x10, %0\n"
    ".word 0x00050008\n" : : "r"(addr) : "x10");
}

// Full cache flush
INLINE void pebblesCacheFlushFull()
{
  // Flush each line
  uint32_t numLines = 1 << SBDCacheLogLines;
  for (uint32_t i = 0; i < numLines; i++) {
    uint32_t addr = i * DRAMBeatBytes;
    pebblesCacheFlushLine((void*) addr);
  }
  // Issue a fence to ensure flush is complete
  pebblesFence();
}

#endif
