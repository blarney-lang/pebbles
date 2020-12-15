// Boot loader
#include <cpu.h>
#include <SoC.h>

// Receive 32-bit word from UART (blocking)
uint32_t getWord()
{
  uint32_t w = 0;
  while (!cpuUartCanGet()) {}; w = cpuUartGet();
  while (!cpuUartCanGet()) {}; w |= (cpuUartGet() << 8);
  while (!cpuUartCanGet()) {}; w |= (cpuUartGet() << 16);
  while (!cpuUartCanGet()) {}; w |= (cpuUartGet() << 24);
  return w;
}

// Boot loader funtionaltiy
int main()
{
  // Receive code (blocking)
  while (1) {
    uint32_t addr = getWord();
    if (addr == 0xffffffff) break;
    uint32_t data = getWord();
    cpuWriteInstr(addr, data);
  }

  // Receive data (blocking)
  while (1) {
    uint32_t addr = getWord();
    if (addr == 0xffffffff) break;
    uint32_t data = getWord();
    volatile uint32_t* ptr = (uint32_t*) addr;
    *ptr = data;
  }

  // Call the application's main function
  int (*appMain)() = (int (*)()) (MaxBootImageBytes);
  appMain();

  // Send terminating null
  while (!cpuUartCanPut()) {}; cpuUartPut('\0');

  // Restart boot loader
  asm volatile("jr zero");

  // Unreachable
  return 0;
}
