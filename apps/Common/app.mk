PEBBLES_ROOT ?= $(realpath ../..)

# Binary utilities
RV_ARCH    = rv32ima
RV_C       = riscv64-unknown-elf-gcc
RV_CC      = riscv64-unknown-elf-g++
RV_LD      = riscv64-unknown-elf-ld
RV_OBJCOPY = riscv64-unknown-elf-objcopy

# Compiler and linker flags for code running on the SoC
CFLAGS  = -mabi=ilp32 -march=$(RV_ARCH) -Os \
          -I $(PEBBLES_ROOT)/inc \
          -static -mcmodel=medany \
          -fvisibility=hidden -nostdlib -nostartfiles \
          -fsingle-precision-constant -fno-builtin-printf \
          -ffp-contract=off -fno-builtin -ffreestanding \
          -fno-reorder-blocks
LDFLAGS = -melf32lriscv -G 0 

OFILES = $(patsubst %.cpp,%.o,$(APP_CPP)) \
         $(PEBBLES_ROOT)/lib/cpu/io.o \
         $(PEBBLES_ROOT)/lib/baremetal.o

.PHONY: all
all: Run

code.v: app.elf
	$(RV_OBJCOPY) -O verilog --only-section=.text app.elf code.v

data.v: app.elf
	$(RV_OBJCOPY) -O verilog --remove-section=.text \
                --set-section-flags .bss=alloc,load,contents app.elf data.v

app.elf: $(OFILES) link.ld
	$(RV_LD) $(LDFLAGS) -T link.ld -o app.elf $(OFILES)

$(PEBBLES_ROOT)/lib/baremetal.o: $(PEBBLES_ROOT)/lib/baremetal.c
	$(RV_C) $(CFLAGS) -I $(PEBBLES_ROOT)/inc -Wall -c -o $@ $<

%.o: %.cpp $(APP_HDR)
	$(RV_CC) $(CFLAGS) -I $(PEBBLES_ROOT)/inc -Wall -c -o $@ $<

link.ld: $(PEBBLES_ROOT)/apps/Common/link.ld.h
	cpp -P -I $(PEBBLES_ROOT)/inc $< > link.ld

Run: checkenv code.v data.v $(RUN_CPP) $(RUN_H)
	g++ -std=c++11 -O2 -I $(PEBBLES_ROOT)/inc -o Run $(RUN_CPP) \
    -fno-exceptions -ljtag_atlantic -ljtag_client \
    -L $(QUARTUS_ROOTDIR)/linux64/ -Wl,-rpath,$(QUARTUS_ROOTDIR)/linux64

RunSim: code.v data.v $(RUN_CPP) $(RUN_H)
	g++ -DSIMULATE -O2 -I $(PEBBLES_ROOT)/inc -o RunSim $(RUN_CPP)

# Raise error if QUARTUS_ROOTDIR not set
.PHONY: checkenv
checkenv:
	$(if $(value QUARTUS_ROOTDIR), , $(error Please set QUARTUS_ROOTDIR))

.PHONY: clean
clean:
	rm -f *.o *.elf link.ld code.v data.v Run RunSim
