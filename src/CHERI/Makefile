CapLibBase.hs: cheri-cap-lib/Makefile
	make -C cheri-cap-lib
	cd cheri-cap-lib && \
    CHERICapWrapBlarney.py CHERI.CapLibBase > ../CapLibBase.hs
	mkdir -p Verilog
	cp cheri-cap-lib/*.v Verilog

cheri-cap-lib/Makefile:
	git submodule update --init --recursive

.PHONY: clean
clean:
	make -C cheri-cap-lib clean clean-verilog-wrappers clean-blarney-wrappers

.PHONY: mrproper
mrproper: clean
	rm -rf CapLibBase.hs Verilog
