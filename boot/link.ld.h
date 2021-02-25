#include <SoC.h>

OUTPUT_ARCH( "riscv" )

/* Base of stack is toward the end of DRAM, before the SIMT thread stacks */
__stackBase =
  (1 << (DRAMAddrWidth + DRAMBeatLogBytes)) -
    (1 << (SIMTLogLanes + SIMTLogWarps + SIMTLogBytesPerStack)) -
      (1 << (SIMTLogLanes + SIMTLogWordsPerSRAMBank+2)) - 4;

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
