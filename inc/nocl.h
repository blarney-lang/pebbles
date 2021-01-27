#ifndef _NOCL_H_
#define _NOCL_H_

#include <SoC.h>
#include <simt.h>
#include <cpu.h>
#include <stdint.h>

#define shared __attribute__ ((aligned (SIMTLanes * 4)))

struct Kernel {
  // Work item id
  uint32_t id;
  // Number of work items
  uint32_t numWorkItems;
};

// SIMT main function
template <typename K> __attribute__ ((noinline)) void _noclSIMTMain_() {
  // Get pointer to kernel closure
  K* kernelPtr = (K*) simtGetKernelClosureAddr();
  K k = *kernelPtr;

  // Initialise the work item id
  k.id = simtThreadId();

  // Execute kernel for every work item
  while (k.id < k.numWorkItems) {
    k.kernel();
    k.id += SIMTLanes * SIMTWarps;
  }

  // Issue a load to ensure all data has reached DRAM
  volatile uint32_t* ptr = &kernelPtr->numWorkItems; *ptr;

  // Terminate warp
  simtWarpTerminate();
}

// SIMT entry point
template <typename K> __attribute__ ((noinline))
  void _noclSIMTEntry_() {
    // Determine stack pointer based on SIMT thread id
    uint32_t top = 0;
    top -= simtThreadId() << SIMTLogBytesPerStack;
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

#endif
