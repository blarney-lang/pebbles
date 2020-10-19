# Pebbles

Pebbles is a RISC-V processor framework supporting plugable pipelines.
The [instruction set](src/Pebbles/Instructions) and
[pipelines](src/Pebbles/Pipeline) are defined separately, with the
idea being that we can define multiple different pipelines for the
same instruction set definition, bringing new levels of modularity and
reusability to processor development.  We do this with the help of a
modern HDL called [Blarney](https://github.com/blarney-lang/blarney).

At the moment, Pebbles is very basic.  It provides a single RV32IM
core with a 5-stage scalar pipeline, tightly coupled internal
instruction and data memories, and a single 8-bit stream interface to
the external world.

## Build instructions

You will need Verilator, the RISC-V SDK, and a fairly recent version
of GHC (8.6.1 or later).

On Ubuntu 20.04, you can do:

```sh
> sudo apt install verilator
> sudo apt install gcc-riscv64-unknown-elf
> sudo apt install ghc-8.6.5
```

Now, recursively clone the repo:

```sh
> git clone --recursive https://github.com/blarney-lang/pebbles
```

Inside the repo, there are various build options.  For example,
running

```sh
> make test
```

will generate Verilog for the scalar RV32IM core, compile it with
Verilator, and then run the RISC-V test suite on the resulting
simulator.

You can also run [this C program](boot/main.c) through the simulator
as follows.

```sh
> make sim
> cd sim
> ./SimPebbles
hello world
```

The full set of build option is:

  * `make verilog` - generate verilog for the scalar RV32IM core.
  * `make sim` - build a Verilator simulator for the core.
  * `make test` - run the RISC-V test suite on the core.
  * `make -C boot` - build the boot loader for the core.
  * `make -C de5-net` - build an FPGA image for the DE5-Net board.
