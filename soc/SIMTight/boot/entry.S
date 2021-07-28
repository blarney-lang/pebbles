#include <Config.h>

#define DRAM_SIZE \
  (1 << (DRAMAddrWidth + DRAMBeatLogBytes))
#define SIMT_STACKS_SIZE \
  (1 << (SIMTLogLanes + SIMTLogWarps + SIMTLogBytesPerStack))
#define BANKED_SRAMS_SIZE \
  (1 << (SIMTLogLanes + SIMTLogWordsPerSRAMBank+2))

// Base of stack is toward the end of DRAM, before the SIMT thread stacks
#define STACK_BASE (DRAM_SIZE - SIMT_STACKS_SIZE - BANKED_SRAMS_SIZE - 8)

.global _start
_start:

# Set stack pointer
#if EnableCHERI
  # TODO: Constrain the bounds/permssions of the stack pointer
  cspecialr csp, ddc
  li x1, STACK_BASE
  csetaddr csp, csp, x1
#else
  li sp, STACK_BASE
#endif

# Jump to main
#if EnableCHERI
  cllc c1, main
  cjalr cra, c1
#else
  jal main
#endif

# Terminate simulator
csrw 0x801, zero

# Loop forever
j .
