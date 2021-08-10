# Fetch blarney repo if it's not there
blarney/Makefile:
	git submodule update --init --recursive

.PHONY: clean
clean:
	make -C lib clean
	make -C soc/SIMTight clean
