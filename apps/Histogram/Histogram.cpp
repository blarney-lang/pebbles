#include <nocl.h>

// Kernel for computing 256-bin histograms
struct Histogram : Kernel {
  int len;
  unsigned char* input;
  int* bins;

  void kernel() {
    // Store histogram bins in shared local memory
    int* histo = shared.array<int, 256>();

    // Initialise bins
    for (int i = threadIdx.x; i < 256; i += blockDim.x)
      histo[i] = 0;

    __syncthreads();

    // Update bins
    for (int i = threadIdx.x; i < len; i += blockDim.x)
      atomicAdd(&histo[input[i]], 1);

    __syncthreads();

    // Write bins to global memory
    for (int i = threadIdx.x; i < 256; i += blockDim.x)
      bins[i] = histo[i];
  }
};

// Vector size for benchmarking
#define N 3000

int main()
{
  // Input and output vectors
  nocl_aligned unsigned char input[N];
  nocl_aligned int bins[256];

  // Initialise inputs
  for (int i = 0; i < N; i++) {
    input[i] = i & 0xff;
  }

  // Instantiate kernel
  Histogram k;

  // Use single block of threads
  k.blockDim.x = SIMTLanes * SIMTWarps;

  // Assign parameters
  k.len = N;
  k.input = input;
  k.bins = bins;

  // Invoke kernel
  noclRunKernel(&k);

  // Display result
  for (int i = 0; i < 256; i++) printf("%x\n", bins[i]);

  return 0;
}
