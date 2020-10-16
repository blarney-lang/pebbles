PEBBLES_ROOT ?= $(realpath ..)
BLARNEY_ROOT = $(PEBBLES_ROOT)/blarney

SimPebbles: SimPebbles.v data.hex prog.hex SimPebbles.cpp
	verilator -cc SimPebbles.v -exe SimPebbles.cpp -o SimPebbles -Wno-UNSIGNED -y $(BLARNEY_ROOT)/Verilog --x-assign unique --x-initial unique
	make -C obj_dir -j -f VSimPebbles.mk SimPebbles
	cp obj_dir/SimPebbles .
	rm -rf obj_dir

SimPebbles.v:
	make -C $(PEBBLES_ROOT)/src

data.hex:
	make -C $(PEBBLES_ROOT)/boot

prog.hex:
	make -C $(PEBBLES_ROOT)/boot

.PHONY: clean 
clean:
	rm -rf SimPebbles *.hex obj_dir SimPebbles.v Pebbles.v
