#ifndef _PEBBLES_CSRS_CYCLECOUNT_H_
#define _PEBBLES_CSRS_CYCLECOUNT_H_

#include <Pebbles/Common.h>

// Control/status registers
#define CSR_CycleCount  "0xc00"
#define CSR_CycleCountH "0xc80"

// Read cycle count (low bits)
INLINE unsigned pebblesCycleCountL()
{
  unsigned x;
  asm volatile ("csrrw %0, " CSR_CycleCount ", zero" : "=r"(x));
  return x;
}

// Read cycle count (high bits)
INLINE unsigned pebblesCycleCountH()
{
  unsigned x;
  asm volatile ("csrrw %0, " CSR_CycleCountH ", zero" : "=r"(x));
  return x;
}

// Read cycle count (64 bits)
INLINE uint64_t pebblesCycleCount()
{
  uint32_t low, high, high2;
  do {
    high = pebblesCycleCountH();
    low = pebblesCycleCountL();
    high2 = pebblesCycleCountH();
  } while (high != high2);
  uint64_t x = (uint64_t) high << 32;
  return x | low;
}

#endif
