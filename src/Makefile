PEBBLES_ROOT = $(realpath ..)
BLARNEY_ROOT = $(PEBBLES_ROOT)/blarney
BLC = $(BLARNEY_ROOT)/Scripts/blc
BLC_INC = $(PEBBLES_ROOT)/inc

.PHONY: all
all:
	$(BLC) $(BLC_FLAGS) -I$(BLC_INC) Main.hs -o Main
	./Main

clean:
	rm -rf *.o *.hi
	rm -f $(shell find Pebbles -regex ".*\.\(hi\|o\)$$")
	rm -f Main
	rm -f Pebbles.v SIMTExecuteStage.v
