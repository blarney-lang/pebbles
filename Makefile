.PHONY: verilog
verilog: blarney/Makefile
	make -C src

.PHONY: sim
sim: verilog
	make -C sim

.PHONY: clean
clean:
	make -C boot clean
	make -C src clean
	make -C de5-net clean
	make -C de10-pro clean
	make -C sim clean
	make -C lib clean
	make -C apps clean

# Fetch blarney repo if it's not there
blarney/Makefile:
	git submodule update --init --recursive
