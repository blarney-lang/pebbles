#include <nocl.h>
#include <rand.h>

// Kernel for matrix-vector multipliation
struct MatVecMul : Kernel {
  int width, height;
  int *mat, *vecIn, *vecOut;
  
  void kernel() {
    // Partial dot products stored in shared local memory
    int* partial = shared.alloc<int>(blockDim.x);

    for (int y = blockIdx.x; y < height; y += gridDim.x) {
      // Row processed by this block
      int* row = mat + y * width;

      // Compute partial dot products
      int sum = 0;
      for (int x = threadIdx.x; x < width; x += blockDim.x)
        sum += row[x] * vecIn[x];
      partial[threadIdx.x] = sum;
      __syncthreads();

      // Final local reduction
      for (int i = blockDim.x >> 1; i > 0; i >>= 1)  {
        if (threadIdx.x < i)
          partial[threadIdx.x] += partial[threadIdx.x + i];
        __syncthreads();
      }

      // Write dot product to global memory
      if (threadIdx.x == 0) vecOut[y] = partial[0];
    }
  }
};

int main()
{
  // Vector and matrix dimensions for benchmarking
  int width = 128;
  int height = 64;

  // Input and outputs
  simt_aligned int mat[height*width], vecIn[width], vecOut[height];

  // Initialise inputs
  uint32_t seed = 1;
  for (int j = 0; j < width; j++)
    vecIn[j] = rand15(&seed) & 0xff;
  for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++)
      mat[i*width+j] = rand15(&seed) & 0xff;
  }

  // Instantiate kernel
  MatVecMul k;

  // One block of threads per matrix row
  k.blockDim.x = SIMTLanes;
  k.gridDim.x = SIMTWarps;

  // Assign parameters
  k.width = width;
  k.height = height;
  k.mat = mat;
  k.vecIn = vecIn;
  k.vecOut = vecOut;

  // Invoke kernel
  noclRunKernel(&k);

  // Check result
  bool ok = true;
  for (int i = 0; i < height; i++) {
    int sum = 0;
    for (int j = 0; j < width; j++)
      sum += mat[i*width+j] * vecIn[j];
    ok = ok && sum == vecOut[i];
  }

  // Display result
  puts("Self test: ");
  puts(ok ? "PASSED" : "FAILED");
  putchar('\n');

  return 0;
}
