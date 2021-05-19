PEBBLES_ROOT = $(realpath ..)
BLARNEY_ROOT = $(PEBBLES_ROOT)/blarney
BLC = $(BLARNEY_ROOT)/Scripts/blc
BLC_INC = $(PEBBLES_ROOT)/inc
BUILD_DIR = $(PEBBLES_ROOT)/src/build

.PHONY: all
all:
	$(BLC) -fno-cpr-anal $(BLC_FLAGS) -I$(BLC_INC) \
    -hidir $(BUILD_DIR) -odir $(BUILD_DIR) Main.hs -o Main
	./Main

clean:
	rm -rf $(BUILD_DIR)
	rm -f Main *.v
	#rm -f $(shell find Pebbles -regex ".*\.\(hi\|o\)$$")
