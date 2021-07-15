PEBBLES_ROOT = $(realpath ../../..)
export BLARNEY_ROOT = $(PEBBLES_ROOT)/blarney
BLC = $(BLARNEY_ROOT)/Scripts/blc
BLC_INC = -I$(PEBBLES_ROOT)/soc/SIMTight/inc \
          -I$(PEBBLES_ROOT)/src
BUILD_DIR = $(PEBBLES_ROOT)/soc/SIMTight/src/build

.PHONY: all
all:
	$(BLC) -fno-cpr-anal $(BLC_FLAGS) -i$(PEBBLES_ROOT)/src $(BLC_INC) \
    -hidir $(BUILD_DIR) -odir $(BUILD_DIR) Main.hs -o Main
	./Main
	cp *.mif $(PEBBLES_ROOT)/soc/SIMTight/de10-pro
	cp *.hex $(PEBBLES_ROOT)/soc/SIMTight/sim

clean:
	rm -rf $(BUILD_DIR)
	rm -f Main *.v *.mif *.hex
