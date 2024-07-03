#! /usr/bin/env bash
  
# Run after quartus_ipgenerate to remove use of DSP blocks

IPDIR=$(dirname $0)

$IPDIR/rmdsp-post.py $IPDIR/FPMul/altera_fp_functions_*/synth/FPMul_*.vhd
$IPDIR/rmdsp-post.py $IPDIR/FPDiv/altera_fp_functions_*/synth/FPDiv_*.vhd
$IPDIR/rmdsp-post.py $IPDIR/FPSqrt/altera_fp_functions_*/synth/FPSqrt_*.vhd
