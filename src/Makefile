BLARNEY_ROOT = $(realpath ../blarney)
BLC = $(BLARNEY_ROOT)/Scripts/blc

.PHONY: all
all:
	$(BLC) $(BLC_FLAGS) Main.hs -o Pebbles
	./Pebbles
	make -C Pebbles-Verilog
	make -C ../boot

clean:
	make -C ../boot clean
	rm -rf *.o *.hi Pebbles Pebbles-Verilog
