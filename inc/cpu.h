#ifndef _CPU_H_
#define _CPU_H_

#include <stdint.h>

// Control/status registers
#define CSR_SimEmit        "0x800"
#define CSR_SimFinish      "0x801"
#define CSR_UARTCanPut     "0x802"
#define CSR_UARTPut        "0x803"
#define CSR_UARTCanGet     "0x804"
#define CSR_UARTGet        "0x805"
#define CSR_InstrAddr      "0x806"
#define CSR_WriteInstr     "0x807"

#define INLINE inline __attribute__((always_inline))

// Emit word to console (simulation only)
INLINE void cpuEmit(unsigned int x)
{
  asm volatile("csrw " CSR_SimEmit ", %0\n" : : "r"(x));
}

// Terminate simulator (simulation only)
INLINE void cpuFinish()
{
  asm volatile("csrw " CSR_SimFinish ", zero\n" : :);
}

// Can write to UART?
INLINE int cpuUartCanPut()
{
  int x;
  asm volatile("csrrw %0, " CSR_UARTCanPut ", zero" : "=r"(x));
  return x;
}

// Can write to UART?
INLINE int cpuUartCanGet()
{
  int x;
  asm volatile("csrrw %0, " CSR_UARTCanGet ", zero" : "=r"(x));
  return x;
}

// Write to UART; assumes uartCanPut() is true
INLINE void cpuUartPut(char c)
{
  asm volatile("csrw " CSR_UARTPut ", %0\n" : : "r"(c));
}

// Receive from UART; assumes uartCanGet() is true
INLINE int cpuUartGet()
{
  int x;
  asm volatile ("csrrw %0, " CSR_UARTGet ", zero" : "=r"(x));
  return x;
}

// Write to instruction memory
INLINE void cpuWriteInstr(uint32_t addr, uint32_t instr)
{
  asm volatile("csrw " CSR_InstrAddr ", %0\n" : : "r"(addr));
  asm volatile("csrw " CSR_WriteInstr ", %0\n" : : "r"(instr));
}

#endif
