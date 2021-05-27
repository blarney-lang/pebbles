#ifndef _PEBBLES_CSRS_SIM_H_
#define _PEBBLES_CSRS_SIM_H_

#include <Pebbles/Common.h>

// Control/status registers
#define CSR_SimEmit   "0x800"
#define CSR_SimFinish "0x801"

// Emit word to console (simulation only)
INLINE void pebblesSimEmit(unsigned int x)
{
  asm volatile("csrw " CSR_SimEmit ", %0\n" : : "r"(x));
}

// Terminate simulator (simulation only)
INLINE void pebblesSimFinish()
{
  asm volatile("csrw " CSR_SimFinish ", zero\n" : :);
}

#endif
