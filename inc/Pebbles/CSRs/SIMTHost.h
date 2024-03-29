#ifndef _SIMTHOST_H_
#define _SIMTHOST_H_

#include <Pebbles/Common.h>
#include <Pebbles/CSRs/StatIds.h>

// Control/status registers
#define CSR_SIMTHostCanPut           "0x820"
#define CSR_SIMTHostInstrAddr        "0x821"
#define CSR_SIMTHostWriteInstr       "0x822"
#define CSR_SIMTHostStartKernel      "0x823"
#define CSR_SIMTHostCanGet           "0x824"
#define CSR_SIMTHostGet              "0x825"
#define CSR_SIMTHostSetKernel        "0x826"
#define CSR_SIMTHostSetWarpsPerBlock "0x827"
#define CSR_SIMTHostAskStats         "0x828"

// Can issue command to SIMT core?
INLINE int pebblesSIMTCanPut()
{
  int x;
  asm volatile("csrrw %0, " CSR_SIMTHostCanPut ", zero" : "=r"(x));
  return x;
}

// Write to SIMT core's instruction memory; assumes simtHostCanPut() is true
INLINE void pebblesSIMTWriteInstr(uint32_t addr, uint32_t instr)
{
  asm volatile("csrw " CSR_SIMTHostInstrAddr ", %0\n" : : "r"(addr));
  asm volatile("csrw " CSR_SIMTHostWriteInstr ", %0\n" : : "r"(instr));
}

// Set address of kernel closure (where kernel code ptr and args reside)
INLINE void pebblesSIMTSetKernel(uint32_t addr)
{
  asm volatile("csrw " CSR_SIMTHostSetKernel ", %0\n" : : "r"(addr));
}

// Set number of warps per block
// (A block is a group of threads that synchronise on a barrier)
// (A value of 0 indicates all warps)
INLINE void pebblesSIMTSetWarpsPerBlock(uint32_t n)
{
  asm volatile("csrw " CSR_SIMTHostSetWarpsPerBlock ", %0\n" : : "r"(n));
}

// Start a kernel on the SIMT core; assumes simtHostCanPut() is true
INLINE void pebblesSIMTStartKernel(uint32_t pc)
{
  asm volatile("csrw " CSR_SIMTHostStartKernel ", %0\n" : : "r"(pc));
}

// Ask for the value of the given stat counter;
// assumes simtHostCanPut() is true
INLINE void pebblesSIMTAskStats(uint32_t id)
{
  asm volatile("csrw " CSR_SIMTHostAskStats ", %0\n" : : "r"(id));
}

// Can receive response from SIMT core?
// (The SIMT issues a response on kernel completion)
INLINE int pebblesSIMTCanGet()
{
  int x;
  asm volatile("csrrw %0, " CSR_SIMTHostCanGet ", zero" : "=r"(x));
  return x;
}

// Receive response from SIMT core; assumes simtHostCanGet() is true
INLINE int pebblesSIMTGet()
{
  int x;
  asm volatile("csrrw %0, " CSR_SIMTHostGet ", zero" : "=r"(x));
  return x;
}

#endif
