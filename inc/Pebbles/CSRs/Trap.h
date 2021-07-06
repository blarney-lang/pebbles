#ifndef _TRAP_H_
#define _TRAP_H_

#include <Pebbles/Common.h>

// Control/status registers
#define CSR_mepc   "0x341"
#define CSR_mcause "0x342"
#define CSR_mtvec  "0x305"

// Read mepc
INLINE uint32_t mepcGet()
{
  uint32_t x;
  asm volatile("csrrw %0, " CSR_mepc ", zero" : "=r"(x));
  return x;
}

// Read mcause
INLINE uint32_t mcauseGet()
{
  uint32_t x;
  asm volatile("csrrw %0, " CSR_mcause ", zero" : "=r"(x));
  return x;
}

// Set mtvec
INLINE void mtvecSet(uint32_t x)
{
  asm volatile("csrw " CSR_mtvec ", %0\n" : : "r"(x));
}

// Set trap handler
INLINE void setTrapHandler(void (*handler)())
{
  mtvecSet((uint32_t) handler);
}

#endif
