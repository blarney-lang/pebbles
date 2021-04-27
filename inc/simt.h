#ifndef _SIMT_H_
#define _SIMT_H_

#include <stdint.h>

// Control/status registers
#define CSR_SimEmit         "0x800"
#define CSR_SimFinish       "0x801"
#define CSR_WrapCmd         "0x830"
#define CSR_WrapGetKernel   "0x831"
#define CSR_HartId          "0xf14"

// Force inlining for some functions
#define INLINE inline __attribute__((always_inline))

// Arrays should be aligned to support coalescing unit
#define simt_aligned __attribute__ ((aligned (SIMTLanes * 4)))

// All threads in a warp entering a block together will exit together
#define simtBlock(block) { simtPush(); block; simtPop(); }

// Base address of shared local memory
extern char __localBase;

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

// All threads (in a warp) entering a simtPush() together will exit the
// corresponding simtPop() together
INLINE void simtPush()
{
  // Custom instruction
  // Opcode: 0000000 rs2 rs1 000 rd 0001001, with rd=0, rs1=0, rs2=0
  asm volatile(".word 0x00050009\n");
}

// All threads (in a warp) entering a simtPush() together will exit the
// corresponding simtPop() together
INLINE void simtPop()
{
  // Custom instruction
  // Opcode: 0000000 rs2 rs1 001 rd 0001001, with rd=0, rs1=0, rs2=0
  asm volatile(".word 0x00051009\n");
}

// Mark a convergence point
INLINE void simtConverge() {
  simtPop();
  simtPush();
}

// Local memory fence
INLINE void simtLocalMemFence()
{
  // Opcode: 0001 0011 0011 00000 000 00000 0001111
  asm volatile(
    ".word 0x1330000f\n");
}

// Memory fence
INLINE void simtGlobalMemFence()
{
  asm volatile("fence rw, rw");
}

// Terminate current warp; assumes all threads in warp have converged
INLINE void simtWarpTerminateSuccess()
{
  uint32_t code = 3; // Successfull termination
  asm volatile("csrw " CSR_WrapCmd ", %0\n" : : "r"(code));
}

// Terminate current warp; assumes all threads in warp have converged
INLINE void simtWarpTerminateFailure()
{
  uint32_t code = 1; // Unsuccessfull termination
  asm volatile("csrw " CSR_WrapCmd ", %0\n" : : "r"(code));
}

// Barrier synchonrisation; assumes all threads in warp have converged
INLINE void simtLocalBarrier()
{
  asm volatile("csrw " CSR_WrapCmd ", zero\n" : : : "memory");
  simtLocalMemFence();
}

// Get id of calling thread
INLINE int simtThreadId()
{
  int x;
  asm volatile("csrrw %0, " CSR_HartId ", zero" : "=r"(x));
  return x;
}

// Get address of kernel closure (where kernel code ptr and args reside)
INLINE uint32_t simtGetKernelClosureAddr()
{
  uint32_t x;
  asm volatile("csrrw %0, " CSR_WrapGetKernel ", zero" : "=r"(x));
  return x;
}

#endif
