.PHONY: verilog
verilog: blarney/Makefile
	make -C src

.PHONY: sim
sim: verilog
	make -C sim

.PHONY: test
test: sim
	make -C tests test

.PHONY: clean
clean:
	make -C boot clean
	make -C src clean
	make -C de5-net clean
	make -C tests clean
	make -C sim clean

# Fetch blarney repo if it's not there
blarney/Makefile:
	git submodule update --init --recursive
