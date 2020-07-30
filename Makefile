.PHONY: verilog
verilog: blarney-present
	make -C src

.PHONY: sim
sim: blarney-present
	make -C src

.PHONY: test
test: sim
	make -C tests test

.PHONY: clean
clean:
	make -C boot clean
	make -C src clean
	make -C de5-net clean
	make -C tests clean

# Fetch blarney repo if it's not there
.PHONY: blarney-present
blarney-present: blarney/Makefile

blarney/Makefile:
	git submodule update --init --recursive
