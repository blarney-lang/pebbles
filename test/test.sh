#! /usr/bin/env bash

APPS=(
  VecAdd
  VecGCD
  Histogram
  Reduce
  Scan
  Transpose
  MatVecMul
  MatMul
)

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

# Options
# =======

TestSim=
TestFPGA=

# Arguments
# =========

while :
do
  case $1 in
    -h|--help)
      echo "Run test-suite and Pebbles examples"
      echo "  --sim      run in simuatlion (verilator)"
      echo "  --fpga     run on FPGA (de10-pro)"
      exit
      ;;
    --sim)
      TestSim=yup
      ;;
    --fpga)
      TestFPGA=yup
      ;;
    -?*)
      printf 'Ignoring unknown flag: %s\n' "$1" >&2
      ;;
    --)
      shift
      break
      ;;
    *)
      break
  esac
  shift
done

if [ "$TestSim" == "" ] && [ "$TestFPGA" == "" ]; then
  TestSim=yup
fi

# Helper functions
# ================

# Check that last command succeeded
assert() {
  if [ "$2" != "" ]; then
    echo -ne "$2"
  fi
  if [ $1 != 0 ]; then
    echo -e "${RED}FAILED${NC}"
    exit -1
  else
    echo -e "${GREEN}ok${NC}"
  fi
}

# Kill simulator if running
cleanup() {
  if [ "$SIM_PID" != "" ]; then
    kill $SIM_PID
  fi
}

# Preparation
# ===========

# Compile and run the circuit generator
echo -n "Pebbles build: "
make -s -C .. src > /dev/null
assert $?

# Prepare simulator
SIM_PID=
if [ "$TestSim" != "" ]; then
  echo -n "Simulator build: "
  make -s -C .. sim > /dev/null
  assert $?
  echo -n "Starting simulator: "
  pushd . > /dev/null
  cd ../sim
  ./sim &
  SIM_PID=$!
  sleep 1
  popd > /dev/null
  ps -p $SIM_PID > /dev/null
  assert $?
  trap cleanup EXIT
  echo
fi

# Prepare FPGA
if [ "$TestFPGA" != "" ] ; then
  # Check that quartus is in scope
  echo -n "Quartus available: "
  JTAG_CABLE=$(type -P quartus)
  assert $?
  # Build only if there's no bitfile already
  if [ ! -f "../de10-pro/output_files/DE10_Pro.sof" ]; then
     echo -n "Building FPGA image: "
     make -s -C ../de10-pro one > /dev/null
     assert $?
  fi
  # Check that FPGA is visisble
  echo -n "FPGA available: "
  JTAG_CABLE=$(jtagconfig 2> /dev/null | grep DE10-Pro)
  test "$JTAG_CABLE" != ""
  assert $?
  # Program FPGA
  echo -n "Programming FPGA: "
  make -s -C ../de10-pro download-sof > /dev/null
  assert $?
  echo
fi

# Test Suite
# ==========

# In simulation
if [ "$TestSim" != "" ]; then
  echo "Test Suite (Scalar, Simulation)"
  echo "==============================="
  echo
  make -s -C ../apps/TestSuite test-cpu-sim
  assert $? "\nSummary: "
  echo
  echo "Test Suite (SIMT, Simulation)"
  echo "============================="
  echo
  make -s -C ../apps/TestSuite test-simt-sim
  assert $? "\nSummary: "
  echo
fi

# On FPGA
if [ "$TestFPGA" != "" ] ; then
  echo "Test Suite (Scalar, FPGA)"
  echo "========================="
  echo
  make -s -C ../apps/TestSuite test-cpu
  assert $? "\nSummary: "
  echo
  echo "Test Suite (SIMT, FPGA)"
  echo "======================="
  echo
  make -s -C ../apps/TestSuite test-simt
  assert $? "\nSummary: "
  echo
fi

# Sample Apps
# ===========

# In simulation
if [ "$TestSim" != "" ]; then
  echo "Apps (Simulation)"
  echo "================="
  echo
  for APP in ${APPS[@]}; do
    echo -n "$APP (build): "
    make -s -C ../apps/$APP RunSim
    assert $?
    echo -n "$APP (run): "
    OK=$(cd ../apps/$APP && ./RunSim | grep "Self test: PASSED")
    test "$OK" != ""
    assert $?
  done
fi

# On FPGA
if [ "$TestFPGA" != "" ] ; then
  echo "Apps (FPGA)"
  echo "==========="
  echo
  for APP in ${APPS[@]}; do
    echo -n "$APP (build): "
    make -s -C ../apps/$APP Run
    assert $?
    echo -n "$APP (run): "
    OK=$(cd ../apps/$APP && ./Run | grep "Self test: PASSED")
    test "$OK" != ""
    assert $?
  done
fi

echo
echo "All tests passed"
