#ifndef _CPU_H_
#define _CPU_H_

#include <stdint.h>
#include <SoC.h>

// Control/status registers
#define CSR_SimEmit         "0x800"
#define CSR_SimFinish       "0x801"
#define CSR_UARTCanPut      "0x802"
#define CSR_UARTPut         "0x803"
#define CSR_UARTCanGet      "0x804"
#define CSR_UARTGet         "0x805"
#define CSR_InstrAddr       "0x806"
#define CSR_WriteInstr      "0x807"
#define CSR_SIMTCanPut      "0x820"
#define CSR_SIMTInstrAddr   "0x821"
#define CSR_SIMTWriteInstr  "0x822"
#define CSR_SIMTStartKernel "0x823"
#define CSR_SIMTCanGet      "0x824"
#define CSR_SIMTGet         "0x825"
#define CSR_SIMTSetKernel   "0x826"

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

// Can issue command to SIMT core?
INLINE int cpuSIMTCanPut()
{
  int x;
  asm volatile("csrrw %0, " CSR_SIMTCanPut ", zero" : "=r"(x));
  return x;
}

// Write to SIMT core's instruction memory; assumes cpuSIMTCanPut() is true
INLINE void cpuSIMTWriteInstr(uint32_t addr, uint32_t instr)
{
  asm volatile("csrw " CSR_SIMTInstrAddr ", %0\n" : : "r"(addr));
  asm volatile("csrw " CSR_SIMTWriteInstr ", %0\n" : : "r"(instr));
}

// Set address of kernel closure (where kernel code ptr and args reside)
INLINE void cpuSIMTSetKernel(uint32_t addr)
{
  asm volatile("csrw " CSR_SIMTSetKernel ", %0\n" : : "r"(addr));
}

// Start a kernel on the SIMT core; assumes cpuSIMTCanPut() is true
INLINE void cpuSIMTStartKernel(uint32_t pc)
{
  asm volatile("csrw " CSR_SIMTStartKernel ", %0\n" : : "r"(pc));
}

// Can receive response from SIMT core?
// (The SIMT issues a response on kernel completion)
INLINE int cpuSIMTCanGet()
{
  int x;
  asm volatile("csrrw %0, " CSR_SIMTCanGet ", zero" : "=r"(x));
  return x;
}

// Receive response from SIMT core; assumes cpuSIMTCanGet() is true
INLINE int cpuSIMTGet()
{
  int x;
  asm volatile("csrrw %0, " CSR_SIMTGet ", zero" : "=r"(x));
  return x;

}

// Cache line flush
INLINE void cpuCacheFlushLine(void* addr)
{
  // Custom instruction
  // Opcode: 0000000 rs2 rs1 000 rd 0001000, with rd=0, rs1=x10, rs2=0
  asm volatile(
    "mv x10, %0\n"
    ".word 0x00050008\n" : : "r"(addr) : "x10");
}

// Full cache flush
INLINE void cpuCacheFlushFull()
{
  // Flush each line
  uint32_t numLines = 1 << SBDCacheLogLines;
  for (uint32_t i = 0; i < numLines; i++) {
    uint32_t addr = i * DRAMBeatBytes;
    cpuCacheFlushLine((void*) addr);
  }
  // Issue a load to ensure flush is complete
  uint8_t slot = 0; volatile uint8_t* addr = &slot; *addr;
  // Invalidate loaded data
  cpuCacheFlushLine((void*) addr);
}

#endif
