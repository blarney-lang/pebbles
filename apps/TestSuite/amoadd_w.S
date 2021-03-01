# See LICENSE for license details.

#*****************************************************************************
# amoadd_w.S
#-----------------------------------------------------------------------------
#
# Test amoadd.w instruction.
#

#include "riscv_test.h"
#include "test_macros.h"
#include "SoC.h"

RVTEST_RV32U
RVTEST_CODE_BEGIN

# Use only a single thread for this test
#ifdef _TEST_SIMT_
  csrrw t3, 0xf14, zero
  bnez t3, pass
#endif

#define SRAM_BASE (\
  1 << (DRAMAddrWidth + DRAMBeatLogBytes) - \
  1 << (SIMTLogLanes + SIMTLogWarps + SIMTLogBytesPerStack))

  TEST_CASE(2, a4, 1, \
    li a0, SRAM_BASE; \
    la a3, amo_operand; \
    sw a3, 0(a0); \
    amoadd.w a4, a3, 0(a0); \
  )

  TEST_CASE(3, a4, 2, \
    li a0, SRAM_BASE; \
    la a3, amo_operand; \
    sw a3, 0(a0); \
    amoadd.w a4, a3, 0(a0); \
  )

  TEST_CASE(4, a4, 3, \
    li a0, SRAM_BASE+4; \
    la a3, amo_operand; \
    sw a3, 0(a0); \
    amoadd.w a4, a3, 0(a0); \
  )

  TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

amo_operand:
    .dword 1

RVTEST_DATA_END
