PEBBLES_ROOT ?= $(realpath ../../..)

# RISC-V subset
RV_ARCH = rv32ima

# Compiler and linker flags
CFLAGS  = -mabi=ilp32 -march=$(RV_ARCH) -O2 -I./inc -I$(PEBBLES_ROOT)/inc
LDFLAGS = -melf32lriscv -G 0

# Can use Clang or GCC
USE_CLANG ?= false

ifeq ($(USE_CLANG), true)
CFLAGS    += --target=riscv64-unknown -mno-relax
RV_CC      = clang++-11
RV_LD      = ld.lld-11
RV_OBJCOPY = riscv64-unknown-elf-objcopy
else
RV_CC      = riscv64-unknown-elf-g++
RV_LD      = riscv64-unknown-elf-ld
RV_OBJCOPY = riscv64-unknown-elf-objcopy
endif

.PHONY: help
help:
	@echo "Targets:"
	@echo "  test-cpu        test CPU core on FPGA"
	@echo "  test-cpu-sim    test CPU core in simulation"
	@echo "  test-simt       test SIMT core on FPGA"
	@echo "  test-simt-sim   test SIMT core in simulation"
	@echo "  clean           remove intermediate files"

# Helper function
v-files-for = $(patsubst %.S,%.$(1).code.v,$(wildcard $(2)/*.S)) \
              $(patsubst %.S,%.$(1).data.v,$(wildcard $(2)/*.S))

.PHONY: v-files
v-files: $(call v-files-for,cpu,I) \
         $(call v-files-for,simt,I) \
         $(call v-files-for,cpu,M) \
         $(call v-files-for,simt,M) \
         $(call v-files-for,simt,A)

%.code.v: %.elf
	@$(RV_OBJCOPY) -O verilog --only-section=.text $< $@

%.data.v: %.elf
	@$(RV_OBJCOPY) -O verilog --remove-section=.text \
                 --set-section-flags .bss=alloc,load,contents $< $@

.PRECIOUS: %.elf
%.elf: %.o link.ld
	@$(RV_LD) $(LDFLAGS) -T link.ld -o $@ $<

link.ld: $(PEBBLES_ROOT)/SIMTight/apps/Common/link.ld.h
	@cpp -P -I $(PEBBLES_ROOT)/inc $< > link.ld

.PRECIOUS: %.cpu.o
%.cpu.o: %.S
	@$(RV_CC) $(CFLAGS) -Wall -c -o $@ $<

.PRECIOUS: %.simt.o
%.simt.o: %.S
	@$(RV_CC) -D_TEST_SIMT_ $(CFLAGS) -Wall -c -o $@ $<

TestCPU: checkenv TestCPU.cpp
	@g++ -std=c++11 -O2 -I $(PEBBLES_ROOT)/inc -o TestCPU TestCPU.cpp \
    -fno-exceptions -ljtag_atlantic -ljtag_client \
    -L $(QUARTUS_ROOTDIR)/linux64/ -Wl,-rpath,$(QUARTUS_ROOTDIR)/linux64

TestCPUSim: TestCPU.cpp
	@g++ -std=c++11 -DSIMULATE -O2 -I $(PEBBLES_ROOT)/inc \
    -o TestCPUSim TestCPU.cpp

TestSIMT: checkenv TestSIMT.cpp
	@g++ -std=c++11 -O2 -I $(PEBBLES_ROOT)/inc -o TestSIMT TestSIMT.cpp \
    -fno-exceptions -ljtag_atlantic -ljtag_client \
    -L $(QUARTUS_ROOTDIR)/linux64/ -Wl,-rpath,$(QUARTUS_ROOTDIR)/linux64

TestSIMTSim: TestSIMT.cpp
	@g++ -std=c++11 -DSIMULATE -O2 -I $(PEBBLES_ROOT)/inc \
    -o TestSIMTSim TestSIMT.cpp

.PHONY: test-cpu
test-cpu: v-files TestCPU
	@./TestCPU I/*.S M/*.S

.PHONY: test-cpu-sim
test-cpu-sim: v-files TestCPUSim
	@./TestCPUSim I/*.S M/*.S

.PHONY: test-simt
test-simt: v-files TestSIMT
	@./TestSIMT I/*.S M/*.S A/*.S

.PHONY: test-simt-sim
test-simt-sim: v-files TestSIMTSim
	@./TestSIMTSim I/*.S M/*.S A/*.S

# Raise error if QUARTUS_ROOTDIR not set
.PHONY: checkenv
checkenv:
	$(if $(value QUARTUS_ROOTDIR), , $(error Please set QUARTUS_ROOTDIR))

.PHONY: clean
clean:
	@rm -f link.ld \
    I/*.o I/*.elf I/*.v \
    M/*.o M/*.elf M/*.v \
    A/*.o A/*.elf A/*.v \
    TestCPU TestCPUSim TestSIMT TestSIMTSim
