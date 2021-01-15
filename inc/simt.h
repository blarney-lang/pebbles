#ifndef _SIMT_H_
#define _SIMT_H_

#include <stdint.h>

// Control/status registers
#define CSR_SimEmit         "0x800"
#define CSR_SimFinish       "0x801"
#define CSR_WrapTerminate   "0x830"
#define CSR_HartId          "0xf14"

#define INLINE inline __attribute__((always_inline))

// Emit word to console (simulation only)
INLINE void simtEmit(unsigned int x)
{
  asm volatile("csrw " CSR_SimEmit ", %0\n" : : "r"(x));
}

// Terminate simulator (simulation only)
INLINE void simtFinish()
{
  asm volatile("csrw " CSR_SimFinish ", zero\n" : :);
}

// Terminate current warp; assumes all threads in warp have converged
INLINE void simtWarpTerminate()
{
  asm volatile("csrw " CSR_WrapTerminate ", zero\n" : :);
}

// Get id of calling thread
INLINE int simtThreadId()
{
  int x;
  asm volatile("csrrw %0, " CSR_HartId ", zero" : "=r"(x));
  return x;
}

#endif
