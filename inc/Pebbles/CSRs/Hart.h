#ifndef _PEBBLES_CSRS_HART_H_
#define _PEBBLES_CSRS_HART_H_

#include <Pebbles/Common.h>

// Control/status registers
#define CSR_HartId "0xf14"

// Get id of calling thread
INLINE int pebblesHartId()
{
  int x;
  asm volatile("csrrw %0, " CSR_HartId ", zero" : "=r"(x));
  return x;
}

#endif
