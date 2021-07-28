#include <Config.h>

OUTPUT_ARCH( "riscv" )

#define DRAM_SIZE \
  (1 << (DRAMAddrWidth + DRAMBeatLogBytes))
#define SIMT_STACKS_SIZE \
  (1 << (SIMTLogLanes + SIMTLogWarps + SIMTLogBytesPerStack))
#define BANKED_SRAMS_SIZE \
  (1 << (SIMTLogLanes + SIMTLogWordsPerSRAMBank+2))

#define DMEM_BASE    (MemBase + (4 << CPUInstrMemLogWords))
#define IMEM_LENGTH  ((4 << CPUInstrMemLogWords) - MaxBootImageBytes)

// SIMT local memory is toward the end of DRAM,
// before SIMT thread stacks and tag memory
__localBase = DRAM_SIZE - SIMT_STACKS_SIZE - BANKED_SRAMS_SIZE;

/* Base of stack is toward the end of DRAM, before SIMT local memory */
__stackBase = __localBase - 4;

MEMORY
{
  /* Define max length of boot loader */
  instrs : ORIGIN = MemBase+MaxBootImageBytes, LENGTH = IMEM_LENGTH
  globals : ORIGIN = DMEM_BASE, LENGTH = 1 << 20
}

SECTIONS
{
  .text   : { *.o(.text*) }             > instrs
  .bss    : { *.o(.bss*) }              > globals = 0
  .rodata : { *.o(.rodata*) }           > globals
  .sdata  : { *.o(.sdata*) }            > globals
  .data   : { *.o(.data*) }             > globals
  .captable : { *.o(.captable*) }       > globals
  __cap_relocs : { *.o(__cap_relocs*) } > globals
  .eh_frame_hdr : ONLY_IF_RW { KEEP (*(.eh_frame_hdr))
                                     *(.eh_frame_hdr.*) } > globals
  .eh_frame : ONLY_IF_RW { KEEP (*(.eh_frame)) *(.eh_frame.*) } > globals
}
