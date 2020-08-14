PEBBLES_ROOT = $(realpath ..)
BLARNEY_ROOT = $(PEBBLES_ROOT)/blarney
BLC = $(BLARNEY_ROOT)/Scripts/blc

.PHONY: all
all:
	$(BLC) $(BLC_FLAGS) Main.hs -odir obj -o Main
	./Main
	make -C verilog
	make -C ../boot

clean:
	make -C ../boot clean
	make -C verilog clean
	rm -rf *.o *.hi obj
	rm -f Main
