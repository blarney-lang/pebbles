#!/bin/bash

export PEBBLES_ROOT=${PEBBLES_ROOT-$(realpath ..)}

SIM=$PEBBLES_ROOT/sim/

if [ ! -f "$SIM/SimPebbles" ]; then
  echo Please build the simulator first
  exit -1
fi

TEST_DIR=$(realpath .)
make --quiet
for FILE in *.S; do
  TEST=$(basename $FILE .S)
  echo -ne "$TEST\t"
  cp $TEST.code.hex $SIM/prog.hex
  cp $TEST.data.hex $SIM/data.hex
  pushd . > /dev/null
  cd $SIM
  RESULT=$(./SimPebbles | head -n 1 | awk '{print $2}')
  popd > /dev/null
  if [ "$RESULT" == "0x00000001" ]; then
    echo "PASSED"
  else
    NUM=$(($RESULT/2))
    echo "FAILED $NUM"
    exit -1
  fi
done
