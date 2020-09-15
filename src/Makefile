PEBBLES_ROOT = $(realpath ..)
BLARNEY_ROOT = $(PEBBLES_ROOT)/blarney
BLC = $(BLARNEY_ROOT)/Scripts/blc

.PHONY: all
all:
	$(BLC) $(BLC_FLAGS) Main.hs -o Main
	./Main
	make -C verilog
	make -C ../boot

clean:
	make -C ../boot clean
	make -C verilog clean
	rm -rf *.o *.hi
	rm -f $(shell find Pebbles -regex ".*\.\(hi\|o\)$$")
	rm -f Main
