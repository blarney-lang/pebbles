#ifndef _PEBBLES_CSRS_UART_H_
#define _PEBBLES_CSRS_UART_H_

#include <Pebbles/Common.h>

// Control/status registers
#define CSR_UARTCanPut "0x802"
#define CSR_UARTPut    "0x803"
#define CSR_UARTCanGet "0x804"
#define CSR_UARTGet    "0x805"

// Can write to UART?
INLINE int pebblesUARTCanPut()
{
  int x;
  asm volatile("csrrw %0, " CSR_UARTCanPut ", zero" : "=r"(x));
  return x;
}

// Can write to UART?
INLINE int pebblesUARTCanGet()
{
  int x;
  asm volatile("csrrw %0, " CSR_UARTCanGet ", zero" : "=r"(x));
  return x;
}

// Write to UART; assumes uartCanPut() is true
INLINE void pebblesUARTPut(char c)
{
  asm volatile("csrw " CSR_UARTPut ", %0\n" : : "r"(c));
}

// Receive from UART; assumes uartCanGet() is true
INLINE int pebblesUARTGet()
{
  int x;
  asm volatile ("csrrw %0, " CSR_UARTGet ", zero" : "=r"(x));
  return x;
}

// Blocking receive from UART
INLINE int pebblesUARTBlockingGet()
{
  while (!pebblesUARTCanGet()) {}
  return pebblesUARTGet();
}

#endif
