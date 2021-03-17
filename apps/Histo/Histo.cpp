#include <nocl.h>
#include <atomics.h>
#include <cpu/io.h>

// Kernel for computing 256-bin histograms
struct Histo256 : Kernel {
  int len;
  unsigned char* input;
  int* bins;

  void kernel() {
    // Store histogram bins in shared local memory
    nocl_local int histo[256];

    // Initialise bins
    for (int i = noclLocalId(); i < 256; i += noclMaxGroupSize())
      histo[i] = 0;

    noclBarrier();

    // Update bins
    for (int i = noclLocalId(); i < len; i += noclMaxGroupSize())
      atomicAdd(&histo[input[i]], 1);

    noclBarrier();

    // Write bins to global memory
    for (int i = noclLocalId(); i < 256; i += noclMaxGroupSize())
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

  // Invoke kernel
  Histo256 k;
  k.len = N;
  k.input = input;
  k.bins = bins;
  noclRunKernel(&k);

  // Display result
  for (int i = 0; i < 256; i++) printf("%x\n", bins[i]);

  return 0;
}
