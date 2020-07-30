PEBBLES_ROOT ?= $(realpath ..)

RTL = $(PEBBLES_ROOT)/src/verilog
QP = $(PEBBLES_ROOT)/de5-net
BIN = $(PEBBLES_ROOT)/tests/

# Binary utilities
RV_ARCH    = rv32i
RV_CC      = riscv64-unknown-elf-gcc
RV_LD      = riscv64-unknown-elf-ld
RV_OBJCOPY = riscv64-unknown-elf-objcopy

# Compiler and linker flags
CFLAGS  = -mabi=ilp32 -march=$(RV_ARCH) -O2 -I. \
          -static -mcmodel=medany \
          -fvisibility=hidden -nostdlib -nostartfiles \
          -fsingle-precision-constant -fno-builtin-printf \
          -ffp-contract=off -fno-builtin
LDFLAGS = -melf32lriscv -G 0 

# C files to compile
HFILES = pebbles.h
CFILES = main.c io.c
OFILES = $(patsubst %.c,%.o,$(CFILES))

# Sizes of instruction and data memories
INSTR_BYTES = 65536
DATA_BYTES  = 65536

.PHONY: all
all: $(RTL)/prog.hex $(QP)/prog.mif \
     $(RTL)/data.hex $(QP)/data.mif

$(RTL)/prog.hex: InstrMem.ihex
	$(BIN)/ihex-to-img.py InstrMem.ihex hex 0 4 $(INSTR_BYTES) 1 \
    > $(RTL)/prog.hex

$(QP)/prog.mif: InstrMem.ihex
	$(BIN)/ihex-to-img.py InstrMem.ihex mif 0 4 $(INSTR_BYTES) 1 \
    > $(QP)/prog.mif

$(RTL)/data.hex: DataMem.ihex
	$(BIN)/ihex-to-img.py DataMem.ihex hex $(INSTR_BYTES) 4 $(DATA_BYTES) 1 \
    > $(RTL)/data.hex

$(QP)/data.mif: DataMem.ihex
	$(BIN)/ihex-to-img.py DataMem.ihex mif $(INSTR_BYTES) 4 $(DATA_BYTES) 1 \
    > $(QP)/data.mif

InstrMem.ihex: out.elf
	$(RV_OBJCOPY) --only-section=.text -O ihex out.elf InstrMem.ihex

DataMem.ihex: out.elf
	$(RV_OBJCOPY) -O ihex --remove-section=.text \
    --set-section-flags .bss=alloc,load,contents out.elf DataMem.ihex

out.elf: entry.S $(OFILES) $(HFILES) link.ld
	$(RV_CC) $(CFLAGS) -Wall -c -o entry.o entry.S
	$(RV_LD) $(LDFLAGS) -T link.ld -o out.elf entry.o $(OFILES)

%.o: %.c $(HFILES)
	$(RV_CC) $(CFLAGS) -Wall -c -o $@ $<

.PHONY: clean
clean:
	rm -f *.o *.elf *.ihex
