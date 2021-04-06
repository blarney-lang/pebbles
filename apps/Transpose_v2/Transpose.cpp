#include <nocl.h>

// Use loops to reduce the thread block size
// (and hence the number of barrier synchronisations)
struct Transpose : Kernel {
  Array2D<int> in, out;
  
  void kernel() {
    Array2D<int> block = shared.array<int>(blockDim.x, blockDim.x + 1);
    
    // Origin of block within matrix
    unsigned originX = blockIdx.x * blockDim.x;
    unsigned originY = blockIdx.y * blockDim.x;
    
    // Load block 
    for (unsigned y = threadIdx.y; y < blockDim.x; y += blockDim.y)
      block[y][threadIdx.x] = in[originY + y][originX + threadIdx.x];
    
    __syncthreads();
    
    // Store block
    for (unsigned y = threadIdx.y; y < blockDim.x; y += blockDim.y)
      out[originX + y][originY + threadIdx.x] = block[threadIdx.x][y];
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

  // Number of loop iterations per block.  The number of iterations
  // times the block Y dimension must equal the block X dimension.
  const int itersPerBlock = 4;

  // Instantiate kernel
  Transpose k;

  // Set block/grid dimensions
  k.blockDim.x = 32;
  k.blockDim.y = 32 / itersPerBlock;
  k.gridDim.x = width / k.blockDim.x;
  k.gridDim.y = height / (itersPerBlock * k.blockDim.y);

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
