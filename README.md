# Pebbles

Pebbles is a RISC-V processor framework supporting *plugable
pipelines*.  Different [pipeline
implementations](src/Pebbles/Pipeline) may be connected to the same
[instruction set implementation](src/Pebbles/Instructions) by way of a
common [pipeline interface](src/Pebbles/Pipeline/Interface.hs).
Pebbles is written in Haskell, using the
[Blarney](https://github.com/blarney-lang/blarney) library.

Currently, Pebbles supports the `RV32IMAxCHERI` instruction set and
two pipelines:

  * A 5-stage in-order [scalar pipeline](src/Pebbles/Pipeline/Scalar.hs).

  * An 8-stage [SIMT pipeline](src/Pebbles/Pipeline/SIMT.hs) (Single
    Instruction Multiple Threads) with a parameterisable number
    of warps and warp size.

Currently, the only application of the Pebbles framework is the
[SIMTight](https://github.com/CTSRD-CHERI/SIMTight) GPGPU being
developed on the CAPcelerate project.
