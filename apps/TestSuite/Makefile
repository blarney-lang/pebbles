PEBBLES_ROOT ?= $(realpath ../..)

# Binary utilities
RV_ARCH    = rv32im
RV_CC      = riscv64-unknown-elf-gcc
RV_LD      = riscv64-unknown-elf-ld
RV_OBJCOPY = riscv64-unknown-elf-objcopy

# Compiler and linker flags
CFLAGS  = -mabi=ilp32 -march=$(RV_ARCH) -O2 -I.
LDFLAGS = -melf32lriscv -G 0 

.PHONY: v-files
v-files: $(patsubst %.S,%.code.v,$(wildcard *.S)) \
         $(patsubst %.S,%.data.v,$(wildcard *.S))

%.code.v: %.elf
	$(RV_OBJCOPY) -O verilog --only-section=.text $< $@

%.data.v: %.elf
	$(RV_OBJCOPY) -O verilog --remove-section=.text \
                --set-section-flags .bss=alloc,load,contents $< $@

.PRECIOUS: %.elf
%.elf: %.o link.ld
	$(RV_LD) $(LDFLAGS) -T link.ld -o $@ $<

link.ld: $(PEBBLES_ROOT)/apps/Common/link.ld.h
	cpp -P -I $(PEBBLES_ROOT)/inc $< > link.ld

.PRECIOUS: %.o
%.o: %.S test_macros.h riscv_test.h encoding.h
	$(RV_CC) $(CFLAGS) -Wall -c -o $@ $<

run: checkenv Run.cpp
	g++ -std=c++11 -O2 -I $(PEBBLES_ROOT)/inc -o run Run.cpp \
    -fno-exceptions -ljtag_atlantic -ljtag_client \
    -L $(QUARTUS_ROOTDIR)/linux64/ -Wl,-rpath,$(QUARTUS_ROOTDIR)/linux64

sim: Run.cpp
	g++ -DSIMULATE -O2 -I $(PEBBLES_ROOT)/inc -o sim Run.cpp

.PHONY: test
test: v-files run
	./run *.S

.PHONY: test-sim
test-sim: v-files sim
	./sim *.S

# Raise error if QUARTUS_ROOTDIR not set
.PHONY: checkenv
checkenv:
	$(if $(value QUARTUS_ROOTDIR), , $(error Please set QUARTUS_ROOTDIR))

.PHONY: clean
clean:
	rm -f *.o *.elf *.out *.v link.ld run sim
