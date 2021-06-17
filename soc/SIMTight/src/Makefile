PEBBLES_ROOT = $(realpath ../../..)
export BLARNEY_ROOT = $(PEBBLES_ROOT)/blarney
BLC = $(BLARNEY_ROOT)/Scripts/blc
BLC_INC = $(PEBBLES_ROOT)/soc/SIMTight/inc
BUILD_DIR = $(PEBBLES_ROOT)/soc/SIMTight/src/build

.PHONY: all
all:
	$(BLC) -fno-cpr-anal $(BLC_FLAGS) -i$(PEBBLES_ROOT)/src -I$(BLC_INC) \
    -hidir $(BUILD_DIR) -odir $(BUILD_DIR) Main.hs -o Main
	./Main

clean:
	rm -rf $(BUILD_DIR)
	rm -f Main *.v
