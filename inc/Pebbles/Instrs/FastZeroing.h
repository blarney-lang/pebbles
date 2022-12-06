#ifndef _PEBBLES_FASTZEROING_H_
#define _PEBBLES_FASTZEROING_H_

#include <Pebbles/Common.h>
#include <Pebbles/Instrs/Fence.h>

// Fast zero instruction
// Address must be aligned on beat boundary
INLINE void pebblesFastZero(uint32_t beatAddr, uint32_t numBeats)
{
  // Custom instruction
  // Opcode: 0000001 rs2 rs1 000 rd 0001000, with rd=0, rs1=x10, rs2=x11
  asm volatile(
    "mv x10, %0\n"
    "mv x11, %1\n"
    ".word 0x02b50008\n" : : "r"(beatAddr), "r"(numBeats) : "x10", "x11");
}

#endif
