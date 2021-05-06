PEBBLES_ROOT ?= $(realpath ../..)

# RISC-V subset
RV_ARCH = rv32ima

# Can use Clang or GCC
USE_CLANG ?= false

ifeq ($(USE_CLANG), true)
CFLAGS     = --target=riscv64-unknown -ffunction-sections
LDFLAGS    = --entry _Z4mainv
RV_CC      = clang-11
RV_LD      = ld.lld-11
RV_OBJCOPY = riscv64-unknown-elf-objcopy
else
CFLAGS     = 
LDFLAGS    = --entry main
RV_CC      = riscv64-unknown-elf-gcc
RV_LD      = riscv64-unknown-elf-ld
RV_OBJCOPY = riscv64-unknown-elf-objcopy
endif

# Compiler and linker flags for code running on the SoC
CFLAGS := $(CFLAGS) -mabi=ilp32 -march=$(RV_ARCH) -O2 \
         -I $(PEBBLES_ROOT)/inc \
         -static -mcmodel=medany \
         -fvisibility=hidden -nostdlib \
         -fno-builtin-printf -ffp-contract=off \
         -fno-builtin -ffreestanding
LDFLAGS := $(LDFLAGS) -melf32lriscv -G 0

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
	$(RV_CC) $(CFLAGS) -I $(PEBBLES_ROOT)/inc -Wall -c -o $@ $<

%.o: %.cpp $(APP_HDR)
	$(RV_CC) $(CFLAGS) -I $(PEBBLES_ROOT)/inc -Wall -c -o $@ $<

link.ld: $(PEBBLES_ROOT)/apps/Common/link.ld.h
	cpp -P -I $(PEBBLES_ROOT)/inc $< > link.ld

Run: checkenv code.v data.v $(RUN_CPP) $(RUN_H)
	gcc -std=c++11 -O2 -I $(PEBBLES_ROOT)/inc -o Run $(RUN_CPP) \
    -fno-exceptions -ljtag_atlantic -ljtag_client \
    -L $(QUARTUS_ROOTDIR)/linux64/ -Wl,-rpath,$(QUARTUS_ROOTDIR)/linux64

RunSim: code.v data.v $(RUN_CPP) $(RUN_H)
	gcc -DSIMULATE -O2 -I $(PEBBLES_ROOT)/inc -o RunSim $(RUN_CPP)

# Raise error if QUARTUS_ROOTDIR not set
.PHONY: checkenv
checkenv:
	$(if $(value QUARTUS_ROOTDIR), , $(error Please set QUARTUS_ROOTDIR))

.PHONY: clean
clean:
	rm -f *.o *.elf link.ld code.v data.v Run RunSim
