#ifndef _PEBBLES_CSRS_INSTRMEM_H_
#define _PEBBLES_CSRS_INSTRMEM_H_

#include <Pebbles/Common.h>

// Control/status registers
#define CSR_InstrAddr  "0x806"
#define CSR_WriteInstr "0x807"

// Write to instruction memory
INLINE void pebblesInstrMemWrite(uint32_t addr, uint32_t instr)
{
  asm volatile("csrw " CSR_InstrAddr ", %0\n" : : "r"(addr));
  asm volatile("csrw " CSR_WriteInstr ", %0\n" : : "r"(instr));
}

#endif
