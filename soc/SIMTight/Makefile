PEBBLES_ROOT ?= $(realpath ../..)

.PHONY: verilog
verilog: $(PEBBLES_ROOT)/blarney/Makefile
	make -C src

.PHONY: sim
sim: verilog
	make -C sim

.PHONY: clean
clean:
	make -C boot clean
	make -C apps clean
	make -C src clean
	make -C de10-pro clean
	make -C sim clean

# Fetch blarney repo if it's not there
$(PEBBLES_ROOT)/blarney/Makefile:
	git submodule update --init --recursive
