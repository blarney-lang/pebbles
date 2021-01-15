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
v-files: $(patsubst %.S,%.cpu.code.v,$(wildcard *.S)) \
         $(patsubst %.S,%.cpu.data.v,$(wildcard *.S)) \
         $(patsubst %.S,%.simt.code.v,$(wildcard *.S)) \
         $(patsubst %.S,%.simt.data.v,$(wildcard *.S))

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

.PRECIOUS: %.cpu.o
%.cpu.o: %.S test_macros.h riscv_test.h encoding.h
	$(RV_CC) $(CFLAGS) -Wall -c -o $@ $<

.PRECIOUS: %.simt.o
%.simt.o: %.S test_macros.h riscv_test.h encoding.h
	$(RV_CC) -D_TEST_SIMT_ $(CFLAGS) -Wall -c -o $@ $<

TestCPU: checkenv TestCPU.cpp
	g++ -std=c++11 -O2 -I $(PEBBLES_ROOT)/inc -o TestCPU TestCPU.cpp \
    -fno-exceptions -ljtag_atlantic -ljtag_client \
    -L $(QUARTUS_ROOTDIR)/linux64/ -Wl,-rpath,$(QUARTUS_ROOTDIR)/linux64

TestCPUSim: TestCPU.cpp
	g++ -DSIMULATE -O2 -I $(PEBBLES_ROOT)/inc -o TestCPUSim TestCPU.cpp

TestSIMT: checkenv TestSIMT.cpp
	g++ -std=c++11 -O2 -I $(PEBBLES_ROOT)/inc -o TestSIMT TestSIMT.cpp \
    -fno-exceptions -ljtag_atlantic -ljtag_client \
    -L $(QUARTUS_ROOTDIR)/linux64/ -Wl,-rpath,$(QUARTUS_ROOTDIR)/linux64

TestSIMTSim: TestSIMT.cpp
	g++ -DSIMULATE -O2 -I $(PEBBLES_ROOT)/inc -o TestSIMTSim TestSIMT.cpp

.PHONY: test-cpu
test-cpu: v-files TestCPU
	./TestCPU *.S

.PHONY: test-cpu-sim
test-cpu-sim: v-files TestCPUSim
	./TestCPUSim *.S

.PHONY: test-simt
test-simt: v-files TestSIMT
	./TestSIMT *.S

.PHONY: test-simt-sim
test-simt-sim: v-files TestSIMTSim
	./TestSIMTSim *.S

# Raise error if QUARTUS_ROOTDIR not set
.PHONY: checkenv
checkenv:
	$(if $(value QUARTUS_ROOTDIR), , $(error Please set QUARTUS_ROOTDIR))

.PHONY: clean
clean:
	rm -f *.o *.elf *.out *.v link.ld \
    TestCPU TestCPUSim TestSIMT TestSIMTSim
