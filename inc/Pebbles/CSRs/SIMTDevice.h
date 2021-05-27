#ifndef _PEBBLES_CSRS_SIMTDEVICE_H_
#define _PEBBLES_CSRS_SIMTDEVICE_H_

#include <Pebbles/Common.h>

// Control/status registers
#define CSR_SIMTDeviceWarpCmd   "0x830"
#define CSR_SIMTDeviceGetKernel "0x831"

// Terminate current warp; assumes all threads in warp have converged
INLINE void pebblesWarpTerminateSuccess()
{
  uint32_t code = 3; // Successfull termination
  asm volatile("csrw " CSR_SIMTDeviceWarpCmd ", %0\n" : : "r"(code));
}

// Terminate current warp; assumes all threads in warp have converged
INLINE void pebblesWarpTerminateFailure()
{
  uint32_t code = 1; // Unsuccessfull termination
  asm volatile("csrw " CSR_SIMTDeviceWarpCmd ", %0\n" : : "r"(code));
}

// Barrier synchonrisation; assumes all threads in warp have converged
INLINE void pebblesSIMTBarrierCore()
{
  asm volatile("csrw " CSR_SIMTDeviceWarpCmd ", zero\n" : : : "memory");
}

// Get address of kernel closure (where kernel args reside)
INLINE uint32_t pebblesKernelClosureAddr()
{
  uint32_t x;
  asm volatile("csrrw %0, " CSR_SIMTDeviceGetKernel ", zero" : "=r"(x));
  return x;
}

#endif
