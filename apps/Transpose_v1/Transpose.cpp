#include <nocl.h>

// One thread per matrix element
// Assumption: blockDim.x == blockDim.y
struct Transpose : Kernel {
  Array2D<int> in, out;

  void kernel() {
    Array2D<int> block = shared.array<int>(blockDim.y, blockDim.x + 1);

    // Origin of block within matrix
    unsigned originX = blockIdx.x * blockDim.x;
    unsigned originY = blockIdx.y * blockDim.y;

    // Load block
    block[threadIdx.y][threadIdx.x] =
      in[originY + threadIdx.y][originX + threadIdx.x];

    __syncthreads();

    // Store block
    out[originX + threadIdx.y][originY + threadIdx.x] =
      block[threadIdx.x][threadIdx.y];
  }
};

int main()
{
  // Matrix size for benchmarking
  const int width = 256;
  const int height = 64;

  // Input and output matrix data
  nocl_aligned int matInData[width*height];
  nocl_aligned int matOutData[width*height];

  // Friendly array wrappers
  Array2D<int> matIn(matInData, height, width);
  Array2D<int> matOut(matOutData, width, height);

  // Initialise inputs
  for (int i = 0; i < height; i++) {
    for (int j = 0; j < width; j++)
      matIn[i][j] = j;
  }

  // Instantiate kernel
  Transpose k;

  // One thread per matrix element
  k.blockDim.x = 32;
  k.blockDim.y = 32;
  k.gridDim.x = width / k.blockDim.x;
  k.gridDim.y = height / k.blockDim.y;

  // Assign parameters
  k.in = matIn;
  k.out = matOut;

  // Invoke kernel
  noclRunKernel(&k);

  // Display result
  for (int i = 0; i < width; i++) {
    for (int j = 0; j < height; j++)
      printf("%x ", matOut[i][j]);
    printf("\n");
  }

  return 0;
}
