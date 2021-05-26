#ifndef _PEBBLES_INSTRS_SIMTDEVICE_H_
#define _PEBBLES_INSTRS_SIMTDEVICE_H_

#include <Pebbles/Common.h>
#include <Pebbles/CSRs/SIMTDevice.h>

// Arrays should be aligned to support coalescing unit
#define simt_aligned __attribute__ ((aligned (SIMTLanes * 4)))

// All threads (in a warp) entering a push together will exit the
// corresponding pop together
INLINE void pebblesSIMTPush()
{
  // Custom instruction
  // Opcode: 0000000 rs2 rs1 000 rd 0001001, with rd=0, rs1=0, rs2=0
  asm volatile(".word 0x00050009\n");
}

INLINE void pebblesSIMTPop()
{
  // Custom instruction
  // Opcode: 0000000 rs2 rs1 001 rd 0001001, with rd=0, rs1=0, rs2=0
  asm volatile(".word 0x00051009\n");
}

// Mark a convergence point
INLINE void pebblesSIMTConverge() {
  pebblesSIMTPop();
  pebblesSIMTPush();
}

// Local memory fence
INLINE void pebblesSIMTLocalMemFence()
{
  // Opcode: 0001 0011 0011 00000 000 00000 0001111
  asm volatile(
    ".word 0x1330000f\n");
}

// Local barrier; assumes all threads in warp have converged
INLINE void pebblesSIMTLocalBarrier()
{
  pebblesSIMTLocalMemFence();
  pebblesSIMTBarrierCore();
}

#endif
