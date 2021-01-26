#include <SoC.h>

#define DMEM_BASE    (4 * (1 << CPUInstrMemLogWords))
#define IMEM_LENGTH  (DMEM_BASE - MaxBootImageBytes)

OUTPUT_ARCH( "riscv" )

/* Base of stack is toward the end of DRAM, before the SIMT thread stacks */
__stackBase =
  (1 << (DRAMAddrWidth + DRAMBeatLogBytes)) -
    (1 << (SIMTLogLanes + SIMTLogWarps + SIMTLogBytesPerStack)) - 4;

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
