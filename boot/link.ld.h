#include <SoC.h>

OUTPUT_ARCH( "riscv" )

/* Base of stack is at the end of DRAM */
__stackBase = ((1 << DRAMAddrWidth) * DRAMBeatBytes) - 4;

MEMORY
{
  /* Define max length of boot loader */
  boot : ORIGIN = 0, LENGTH = MaxBootImageBytes
}

SECTIONS
{
  /* Instruction memory */
  /* (No data sections allowed in boot loader) */
  .text : { *.o(.text*) } > boot
}
