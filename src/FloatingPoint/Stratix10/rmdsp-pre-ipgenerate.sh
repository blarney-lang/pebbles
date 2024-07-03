#! /usr/bin/env bash
  
# Run before quartus_ipgenerate to remove use of DSP blocks

IPDIR=$(dirname $0)

$IPDIR/rmdsp-pre.py $IPDIR/FPAddSub.ip
$IPDIR/rmdsp-pre.py $IPDIR/FPMul.ip
$IPDIR/rmdsp-pre.py $IPDIR/FPDiv.ip
$IPDIR/rmdsp-pre.py $IPDIR/FPSqrt.ip
