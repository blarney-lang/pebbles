PEBBLES_ROOT ?= $(realpath ../..)
BLARNEY_ROOT = $(PEBBLES_ROOT)/blarney

SimPebbles: *.v *.cpp SimPebbles.v
	verilator -cc SimPebbles.v -exe SimPebbles.cpp -o SimPebbles -Wno-UNSIGNED -y $(BLARNEY_ROOT)/Verilog --x-assign unique --x-initial unique
	make -C obj_dir -j -f VSimPebbles.mk SimPebbles
	cp obj_dir/SimPebbles .
	rm -rf obj_dir

SimPebbles.v:
	$(error Please generate the verilog first)

.PHONY: clean 
clean:
	rm -rf SimPebbles *.hex obj_dir SimPebbles.v Pebbles.v
