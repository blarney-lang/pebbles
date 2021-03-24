#ifndef _NOCL_H_
#define _NOCL_H_

#include <SoC.h>
#include <simt.h>
#include <cpu.h>
#include <stdint.h>

// Alias some useful SIMT macros
#define nocl_local simt_local
#define nocl_aligned simt_aligned

// Parameters that are available to any kernel
// All kernels inherit from this
struct Kernel {
  // Currently unused
};

// SIMT main function
template <typename K> __attribute__ ((noinline)) void _noclSIMTMain_() {
  // Get pointer to kernel closure
  K* kernelPtr = (K*) simtGetKernelClosureAddr();
  K k = *kernelPtr;

  // Execute kernel
  k.kernel();

  // Issue a fence ensure all data has reached DRAM
  simtGlobalMemFence();

  // Terminate warp
  simtWarpTerminate();
}

// SIMT entry point
template <typename K> __attribute__ ((noinline))
  void _noclSIMTEntry_() {
    // Determine stack pointer based on SIMT thread id
    uint32_t top = 0;
    top -= (SIMTLanes * SIMTWarps - 1 - simtThreadId()) <<
             SIMTLogBytesPerStack;
    top -= 8;
    // Set stack pointer
    asm volatile("mv sp, %0\n" : : "r"(top));
    // Invoke main function
    _noclSIMTMain_<K>();
  }

// Trigger SIMT kernel execution from CPU
template <typename K> __attribute__ ((noinline))
  int noclRunKernel(K* k) {
    // Set address of kernel closure
    uintptr_t kernelAddr = (uintptr_t) k;
    cpuSIMTSetKernel(kernelAddr);

    // Flush cache
    cpuCacheFlushFull();

    // Start kernel on SIMT core
    uintptr_t entryAddr = (uintptr_t) _noclSIMTEntry_<K>;
    while (! cpuSIMTCanPut()) {}
    cpuSIMTStartKernel(entryAddr);

    // Wait for kernel response
    while (!cpuSIMTCanGet()) {}
    return cpuSIMTGet();
  }

// Get local thread id
INLINE int noclLocalId() {
  return simtThreadId();
}

// Max workgroup size
INLINE int noclMaxGroupSize() {
  return SIMTLanes * SIMTWarps;
}

// Barrier synchronisation
INLINE void noclBarrier() {
  simtLocalBarrier();
}

#endif
