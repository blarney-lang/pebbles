CapLib.hs: cheri-cap-lib/Makefile
	make -C cheri-cap-lib
	cd cheri-cap-lib && CHERICapWrapBlarney.py CHERI.CapLib > ../CapLib.hs
	cp cheri-cap-lib/*.v .

cheri-cap-lib/Makefile:
	git submodule update --init --recursive

.PHONY: clean
clean:
	make -C cheri-cap-lib clean clean-verilog-wrappers clean-blarney-wrappers

.PHONY: mrproper
mrproper: clean
	rm -f CapLib.hs *.v
