# Pebbles

Pebbles is a RISC-V processor with plugable pipelines.  The
[instruction set](src/Pebbles/Instructions) and
[pipeline](src/Pebbles/Pipeline) are defined separately, with the idea
being that we can define multiple different pipelines for the same
instruction set, bringing new levels of modularity and reusability to
processor development.  We do this with the help of a modern HDL, in
our case [Blarney](https://github.com/blarney-lang/blarney).

## Build instructions

You will need Verilator, the RISC-V SDK, and a fairly recent version
of GHC (8.6.3 or later).  To install Verilator and the GNU RISC-V SDK
on Ubuntu 20.04:

```
  sudo apt install verilator
  sudo apt install gcc-riscv64-unknown-elf
```

To install GHC 8.6.3 on Ubuntu (assuming you have
[this ppa](https://launchpad.net/~hvr/+archive/ubuntu/ghc/)
configured):

```
  sudo apt install ghc-8.6.3
```

At the moment, Pebbles is very basic.  It consists of a single RV32IM
core with a 5-stage pipeline, tightly coupled internal instruction and
data memories, and a single 8-bit stream interface to the external world!

Then there are various build options for Pebbles:

  * `make verilog` - generate verilog for a scalar RV32IM core.
  * `make sim` - build a Verilator simulator for the core.
  * `make test` - run the RISC-V test suite on the core.
  * `make -C boot` - build the boot loader for the core.
  * `make -C de5-net` - build FPGA image for the DE5-Net board.
