# Pebbles

Pebbles is a RISC-V processor framework supporting *plugable
pipelines*.  Different [pipeline
implementations](src/Pebbles/Pipeline) may be connected to the same
[instruction set implementation](src/Pebbles/Instructions) by way of a
common [pipeline interface](src/Pebbles/Pipeline/Inteface.hs).
Pebbles is written in a modern HDL called
[Blarney](https://github.com/blarney-lang/blarney).

Currently, Pebbles supports the RV32IMA instruction set and two
pipelines:

  * A 5-stage in-order [scalar pipeline](src/Pebbles/Pipeline/Scalar.hs).

  * A 9-stage [Single Instruction Multiple Thread (SIMT) pipeline](src/Pebbles/Pipeline/SIMT.hs)
    with a parameterisable number of warps and warp size.

A sample SoC is included which contains a scalar CPU, a data cache, a
32-lane 64-warp SIMT accelerator, a coalescing unit, and shared DRAM.

<img src="doc/SoC.svg" width="450">

The SoC is optimised for a high MIPS/LUT on FPGA.  Sample projects are
provided for the [DE5-Net](http://de5-net.terasic.com) and
[DE10-Pro](http://de10-pro.terasic.com) development boards.  There is
also a [CUDA-like library](inc/nocl.h) and a set of sample [compute
kernels](apps/).

## Build instructions

We'll need Verilator, the RISC-V SDK, and a fairly recent version
of GHC (8.6.1 or later).

On Ubuntu 20.04, we can simply do:

```sh
$ sudo apt install verilator
$ sudo apt install gcc-riscv64-unknown-elf
$ sudo apt install ghc-8.6.5
```

Now, we recursively clone the repo:

```sh
$ git clone --recursive https://github.com/blarney-lang/pebbles
```

Inside the repo, there are various things to try.  For example, to
build and run the SoC simulator:

```sh
$ cd sim
$ make
$ ./sim
```

While the simulator is running, you can build and run the test suite
in a separate terminal:

```sh
$ cd apps/TestSuite
$ make test-cpu-sim     # Run the RISC-V test suite on the CPU
$ make test-simt-sim    # Run the RISC-V test suite on the SIMT core
```

Alternatively, you can run one of the SIMT kernels:

```sh
$ cd apps/VecAdd
$ make RunSim
$ ./RunSim
```
