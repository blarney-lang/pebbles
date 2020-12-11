PEBBLES_ROOT ?= $(realpath ..)
BLARNEY_ROOT = $(PEBBLES_ROOT)/blarney

sim: Main.cpp ../src/Pebbles.v prog.hex prog.mif
	verilator -cc ../src/Pebbles.v -exe Main.cpp -o sim \
    -Wno-UNSIGNED -y $(BLARNEY_ROOT)/Verilog \
    --x-assign unique --x-initial unique \
    -CFLAGS "-I $(PEBBLES_ROOT)/inc -fmax-errors=1"
	make -C obj_dir -j -f VPebbles.mk sim
	cp obj_dir/sim .
	rm -rf obj_dir

../src/Pebbles.v:
	make -C $(PEBBLES_ROOT)/src

prog.hex:
	make -C $(PEBBLES_ROOT)/boot

prog.mif: prog.hex
	ln -s prog.hex prog.mif

.PHONY: clean 
clean:
	rm -rf sim *.hex *.mif obj_dir
