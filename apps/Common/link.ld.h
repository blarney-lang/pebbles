#include <SoC.h>

#define DMEM_BASE    (4 * (1 << CPUInstrMemLogWords))
#define IMEM_LENGTH  (DMEM_BASE - MaxBootImageBytes)

OUTPUT_ARCH( "riscv" )

/* SIMT local memory is toward the end of DRAM, before SIMT thread stacks */
__localBase =
   (1 << (DRAMAddrWidth + DRAMBeatLogBytes)) -
    (1 << (SIMTLogLanes + SIMTLogWarps + SIMTLogBytesPerStack)) -
      (1 << (SIMTLogLanes + SIMTLogWordsPerSRAMBank+2));

/* Base of stack is toward the end of DRAM, before SIMT local memory */
__stackBase = __localBase - 4;

MEMORY
{
  /* Define max length of boot loader */
  instrs : ORIGIN = MaxBootImageBytes, LENGTH = IMEM_LENGTH
  globals : ORIGIN = DMEM_BASE, LENGTH = 1 << 20
}

SECTIONS
{
  .text   :
     {
       *(.text.startup);
       *(.text*);
     }                                  > instrs
  .bss    : { *.o(.bss*) }              > globals = 0
  .rodata : { *.o(.rodata*) }           > globals
  .sdata  : { *.o(.sdata*) }            > globals
  .data   : { *.o(.data*) }             > globals
  __heapBase = ALIGN(.);
}
