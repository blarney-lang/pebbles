PEBBLES_ROOT ?= $(realpath ..)

SIM = $(PEBBLES_ROOT)/sim
QP = $(PEBBLES_ROOT)/de5-net

# Binary utilities
RV_ARCH    = rv32i
RV_CC      = riscv64-unknown-elf-gcc
RV_LD      = riscv64-unknown-elf-ld
RV_OBJCOPY = riscv64-unknown-elf-objcopy

# Compiler and linker flags
CFLAGS  = -mabi=ilp32 -march=$(RV_ARCH) -Os \
          -I $(PEBBLES_ROOT)/inc \
          -static -mcmodel=medany \
          -fvisibility=hidden -nostdlib -nostartfiles \
          -fsingle-precision-constant -fno-builtin-printf \
          -ffp-contract=off -fno-builtin -ffreestanding
LDFLAGS = -melf32lriscv -G 0 

# C files to compile
CFILES = boot.c
OFILES = $(patsubst %.c,%.o,$(CFILES))

# Size of instruction memory
LOG_INSTR_BYTES = $(shell echo -n CPUInstrMemLogWords \
  | cpp -P -imacros $(PEBBLES_ROOT)/inc/SoC.h - | xargs)
INSTR_BYTES = $(shell echo "4 * (2 ^ $(LOG_INSTR_BYTES))" | bc)

.PHONY: all
all: $(SIM)/boot.hex $(QP)/boot.mif

link.ld: link.ld.h
	cpp -P -I $(PEBBLES_ROOT)/inc link.ld.h > link.ld

$(SIM)/boot.hex: InstrMem.ihex
	./ihex-to-img.py InstrMem.ihex hex 0 4 $(INSTR_BYTES) 1 \
    > $(SIM)/boot.hex

$(QP)/boot.mif: InstrMem.ihex
	./ihex-to-img.py InstrMem.ihex mif 0 4 $(INSTR_BYTES) 1 \
    > $(QP)/boot.mif

InstrMem.ihex: out.elf
	$(RV_OBJCOPY) --only-section=.text -O ihex out.elf InstrMem.ihex

DataMem.ihex: out.elf
	$(RV_OBJCOPY) -O ihex --remove-section=.text \
    --set-section-flags .bss=alloc,load,contents out.elf DataMem.ihex

out.elf: entry.S $(OFILES) link.ld
	$(RV_CC) $(CFLAGS) -Wall -c -o entry.o entry.S
	$(RV_LD) $(LDFLAGS) -T link.ld -o out.elf entry.o $(OFILES)

%.o: %.c $(HFILES)
	$(RV_CC) $(CFLAGS) -Wall -c -o $@ $<

.PHONY: clean
clean:
	rm -f *.o *.elf *.ihex link.ld
