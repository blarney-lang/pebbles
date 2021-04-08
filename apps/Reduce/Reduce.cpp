#include <nocl.h>

// Kernel for vector summation
struct Reduce : Kernel {
  int len;
  int *in, *sum;
  
  void kernel() {
    int* block = shared.alloc<int>(blockDim.x);

    // Sum global memory
    block[threadIdx.x] = 0;
    for (int i = threadIdx.x; i < len; i += blockDim.x)
      block[threadIdx.x] += in[i];

    __syncthreads();

    // Sum shared local memory
    for(int i = blockDim.x >> 1; i > 0; i >>= 1)  {
      if (threadIdx.x < i)
        block[threadIdx.x] += block[threadIdx.x + i];
      __syncthreads();
    }

    // Write sum to global memory
    if (threadIdx.x == 0) *sum = block[0];
  }
};

int main()
{
  // Vector size for benchmarking
  int N = 3000;

  // Input and outputs
  simt_aligned int in[N];
  int sum;

  // Initialise inputs
  for (int i = 0; i < N; i++) in[i] = i;

  // Instantiate kernel
  Reduce k;

  // Use a single block of threads
  k.blockDim.x = SIMTWarps * SIMTLanes;

  // Assign parameters
  k.len = N;
  k.in = in;
  k.sum = &sum;

  // Invoke kernel
  noclRunKernel(&k);

  // Check result
  bool ok = sum == (N*(N-1))/2;

  // Display result
  puts("Self test: ");
  puts(ok ? "PASSED" : "FAILED");
  putchar('\n');

  return 0;
}
