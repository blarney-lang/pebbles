#*****************************************************************************
# csc.S
#-----------------------------------------------------------------------------
#
# Test csc instruction.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_RV32U
RVTEST_CODE_BEGIN

  cgetaddr x3, csp

  # Push stack pointer onto stack
  csc csp, -8(csp)
  # Read it back
  clc c5, -8(csp)

  # Check that tag bit is set
  #TEST_CASE( 2, x3, 0x00000001, cgettag x3, c5; );

  # Check that address is correct
  li TESTNUM, 3
  cgetaddr x3, c5
  cgetaddr x4, csp
  bne x3, x4, fail;

  # Write to capability sub-word
  csw x0, -4(csp)
  # Read it back
  clc c5, -8(csp)

  # Check that tag bit is clear
  TEST_CASE( 4, x3, 0x00000000, cgettag x3, c5; );

  TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
