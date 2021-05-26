PEBBLES_ROOT ?= $(realpath ../../..)
BLARNEY_ROOT = $(PEBBLES_ROOT)/blarney

sim: Main.cpp ../src/Pebbles.v boot.hex boot.mif
	verilator -cc ../src/Pebbles.v -exe Main.cpp -o sim \
    -Wno-UNSIGNED -y $(PEBBLES_ROOT)/soc/SIMTight/src/ \
    -y $(BLARNEY_ROOT)/Verilog \
    --x-assign unique --x-initial unique \
    -CFLAGS "-I $(PEBBLES_ROOT)/inc/Sim -I $(PEBBLES_ROOT)/inc -fmax-errors=1"
	make -C obj_dir -j -f VPebbles.mk sim
	cp obj_dir/sim .
	rm -rf obj_dir

../src/Pebbles.v:
	make -C ../src

boot.hex:
	make -C ../boot

boot.mif: boot.hex
	ln -s boot.hex boot.mif

.PHONY: clean 
clean:
	rm -rf sim *.hex *.mif obj_dir
