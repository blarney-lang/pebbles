#include <Config.h>

OUTPUT_ARCH( "riscv" )

#define DRAM_SIZE \
  (1 << (DRAMAddrWidth + DRAMBeatLogBytes))
#define SIMT_STACKS_SIZE \
  (1 << (SIMTLogLanes + SIMTLogWarps + SIMTLogBytesPerStack))
#define BANKED_SRAMS_SIZE \
  (1 << (SIMTLogLanes + SIMTLogWordsPerSRAMBank+2))

// Base of stack is toward the end of DRAM, before the SIMT thread stacks
__stackBase = DRAM_SIZE - SIMT_STACKS_SIZE - BANKED_SRAMS_SIZE - 4;

MEMORY
{
  /* Define max length of boot loader */
  boot : ORIGIN = MemBase, LENGTH = MaxBootImageBytes
}

SECTIONS
{
  /* Instruction memory */
  /* (No data sections allowed in boot loader) */
  .text : { *.o(.text*) } > boot
}
