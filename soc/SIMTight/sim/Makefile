PEBBLES_ROOT ?= $(realpath ../../..)
BLARNEY_ROOT = $(PEBBLES_ROOT)/blarney
SIM_CFLAGS = -I $(PEBBLES_ROOT)/inc/ \
             -I $(PEBBLES_ROOT)/soc/SIMTight/inc \
             -fmax-errors=1

sim: Main.cpp ../src/SIMTight.v boot.hex boot.mif
	verilator -cc ../src/SIMTight.v -exe Main.cpp -o sim \
    -Wno-UNSIGNED -y $(PEBBLES_ROOT)/soc/SIMTight/src/ \
    -y $(BLARNEY_ROOT)/Verilog \
    -y $(PEBBLES_ROOT)/src/CHERI/Verilog \
    --x-assign unique --x-initial unique \
    -CFLAGS "$(SIM_CFLAGS)"
	make -C obj_dir -j -f VSIMTight.mk sim
	cp obj_dir/sim .
	rm -rf obj_dir

../src/SIMTight.v:
	make -C ../src

boot.hex:
	make -C ../boot

boot.mif: boot.hex
	ln -s boot.hex boot.mif

.PHONY: clean 
clean:
	rm -rf sim *.hex *.mif obj_dir
