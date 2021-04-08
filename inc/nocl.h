// CUDA-like library for compute kernels

#ifndef _NOCL_H_
#define _NOCL_H_

#include <SoC.h>
#include <cpu.h>
#include <simt.h>
#include <stdint.h>
#include <atomics.h>

// Alias some useful SIMT macros
#define nocl_aligned simt_aligned

// Utility functions
// =================

// Return input where only first non-zero bit is set, starting from LSB
inline unsigned firstHot(unsigned x) {
  return x & (~x + 1);
}

// Is the given value a power of two?
inline bool isOneHot(unsigned x) {
  return x > 0 && (x & ~firstHot(x)) == 0;
}

// Compute logarithm (base 2) 
inline unsigned log2floor(unsigned x) {
  unsigned count = 0;
  while (x > 1) { x >>= 1; count++; }
  return count;
}

// Swap the values of two variables
template <typename T> INLINE void swap(T& a, T& b)
  { T tmp = a; a = b; b = tmp; }

// Data types
// ==========

// Dimensions
struct Dim3 {
  int x, y, z;
  Dim3() : x(1), y(1), z(1) {};
  Dim3(int xd) : x(xd), y(1), z(1) {};
  Dim3(int xd, int yd) : x(xd), y(yd), z(1) {};
  Dim3(int xd, int yd, int zd) : x(xd), y(yd), z(zd) {};
};

// 1D arrays
template <typename T> struct Array {
  T* base;
  int size;
  Array() {}
  Array(T* ptr, int n) : base(ptr), size(n) {}
  INLINE T& operator[](int index) const {
    return base[index];
  }
};

// 2D arrays
template <typename T> struct Array2D {
  T* base;
  int size0, size1;
  Array2D() {}
  Array2D(T* ptr, int n0, int n1) :
    base(ptr), size0(n0), size1(n1) {}
  INLINE const Array<T> operator[](int index) const {
    Array<T> a; a.base = &base[index * size1]; a.size = size1; return a;
  }
};

// 3D arrays
template <typename T> struct Array3D {
  T* base;
  int size0, size1, size2;
  Array3D() {}
  Array3D(T* ptr, int n0, int n1, int n2) :
    base(ptr), size0(n0), size1(n1), size2(n2) {}
  INLINE const Array2D<T> operator[](int index) const {
    Array2D<T> a; a.base = &base[index * size1 * size2];
    a.size0 = size1; a.size1 = size2; return a;
  }
};

// For shared local memory allocation
// Memory is allocated/released using a stack
struct SharedLocalMem {
  // This points to the top of the stack (which grows upwards)
  char* top;

  // Allocate memory on shared memory stack (static)
  template <int numBytes> void* alloc() {
    void* ptr = (void*) top;
    constexpr int bytes =
      (numBytes & 3) ? (numBytes & ~3) + 4 : numBytes;
    top += bytes;
    return ptr;
  }

  // Allocate memory on shared memory stack (dynamic)
  void* alloc(int numBytes) {
    void* ptr = (void*) top;
    int bytes = (numBytes & 3) ? (numBytes & ~3) + 4 : numBytes;
    top += bytes;
    return ptr;
  }

  // Typed allocation
  template <typename T> T* alloc(int n) {
    return (T*) alloc(n * sizeof(T));
  }

  // Allocate 1D array with static size
  template <typename T, int dim1> T* array() {
    return (T*) alloc<dim1 * sizeof(T)>();
  }

  // Allocate 2D array with static size
  template <typename T, int dim1, int dim2> auto array() {
    return (T (*)[dim2]) alloc<dim1 * dim2 * sizeof(T)>();
  }

  // Allocate 3D array with static size
  template <typename T, int dim1, int dim2, int dim3> auto array() {
    return (T (*)[dim2][dim3]) alloc<dim1 * dim2 * dim3 * sizeof(T)>();
  }

  // Allocate 1D array with dynamic size
  template <typename T> Array<T> array(int n) {
    Array<T> a; a.base = (T*) alloc(n * sizeof(T));
    a.size = n; return a;
  }

  // Allocate 2D array with dynamic size
  template <typename T> Array2D<T> array(int n0, int n1) {
    Array2D<T> a; a.base = (T*) alloc(n0 * n1 * sizeof(T));
    a.size0 = n0; a.size1 = n1; return a;
  }

  template <typename T> Array3D<T>
    array(int n0, int n1, int n2) {
      Array3D<T> a; a.base = (T*) alloc(n0 * n1 * n2 * sizeof(T));
      a.size0 = n0; a.size1 = n1; a.size2 = n2; return a;
    }
};

// Parameters that are available to any kernel
// All kernels inherit from this
struct Kernel {
  // Blocks per streaming multiprocessor
  unsigned blocksPerSM;

  // Grid and block dimensions
  Dim3 gridDim, blockDim;

  // Block and thread indexes
  Dim3 blockIdx, threadIdx;

  // Shared local memory
  SharedLocalMem shared;
};

// Kernel invocation
// =================

// SIMT main function
// Support only 1D blocks for now
template <typename K> __attribute__ ((noinline)) void _noclSIMTMain_() {
  // Get pointer to kernel closure
  K* kernelPtr = (K*) simtGetKernelClosureAddr();
  K k = *kernelPtr;

  // Block dimensions are all powers of two
  unsigned blockXMask = k.blockDim.x - 1;
  unsigned blockYMask = k.blockDim.y - 1;
  unsigned blockXShift = log2floor(k.blockDim.x);
  unsigned blockYShift = log2floor(k.blockDim.y);

  // Set thread index
  k.threadIdx.x = simtThreadId() & blockXMask;
  k.threadIdx.y = (simtThreadId() >> blockXShift) & blockYMask;
  k.threadIdx.z = 0;

  // Set initial block index
  unsigned blockIdxWithinSM = simtThreadId() >> (blockXShift + blockYShift);
  k.blockIdx.x = blockIdxWithinSM;
  k.blockIdx.y = 0;

  // Set base of shared local memory (per block)
  unsigned localBytes = 4 << (SIMTLogLanes + SIMTLogWordsPerSRAMBank);
  unsigned localBytesPerBlock = localBytes / k.blocksPerSM;

  // Invoke kernel
  while (k.blockIdx.y < k.gridDim.y) {
    while (k.blockIdx.x < k.gridDim.x) {
      k.shared.top = &__localBase + localBytesPerBlock * blockIdxWithinSM;
      k.kernel();
      k.blockIdx.x += k.blocksPerSM;
      simtLocalBarrier();
    }
    simtConverge();
    k.blockIdx.x = blockIdxWithinSM;
    k.blockIdx.y++;
  }

  // Issue a fence ensure all data has reached DRAM
  simtGlobalMemFence();

  // Terminate warp
  simtWarpTerminateSuccess();
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
    unsigned threadsPerBlock = k->blockDim.x * k->blockDim.y;

    // Constraints (some of which are simply limitations)
    cpuAssert(k->blockDim.z == 1,
      "NoCL: blockDim.z != 1 (3D thread blocks not yet supported)");
    cpuAssert(k->gridDim.z == 1,
      "NoCL: gridDim.z != 1 (3D grids not yet supported)");
    cpuAssert(isOneHot(k->blockDim.x) && isOneHot(k->blockDim.y),
      "NoCL: blockDim.x or blockDim.y is not a power of two");
    cpuAssert(threadsPerBlock >= SIMTLanes,
      "NoCL: warp size does not divide evenly into block size");
    cpuAssert(threadsPerBlock <= SIMTWarps * SIMTLanes,
      "NoCL: block size is too large (exceeds SIMT thread count)");

    // Set number of warps per block
    // (for fine-grained barrier synchronisation)
    unsigned warpsPerBlock = threadsPerBlock >> SIMTLogLanes;
    while (!cpuSIMTCanPut()) {}
    cpuSIMTSetWarpsPerBlock(warpsPerBlock);

    // Set number of blocks per streaming multiprocessor
    k->blocksPerSM = (SIMTWarps * SIMTLanes) / threadsPerBlock;
    cpuAssert((k->gridDim.x % k->blocksPerSM) == 0,
      "NoCL: blocks-per-SM does not divide evenly into grid width");

    // Set address of kernel closure
    uintptr_t kernelAddr = (uintptr_t) k;
    while (!cpuSIMTCanPut()) {}
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

// Barrier synchronisation
INLINE void __syncthreads() {
  simtLocalBarrier();
}

#endif
