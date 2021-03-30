# Control/Status Registers (CSRs)

## Standard RISC-V CSRs

  Name              | CSR    | R/W | Function
  ----------------- | ------ | --- | --------
  `HartId`          | 0xf14  | R   | Get hardware thread id

## Custom Simulation CSRs

  Name              | CSR    | R/W | Function
  ----------------- | ------ | --- | --------
  `SimEmit`         | 0x800  | W   | Emit word in simulation
  `SimFinish`       | 0x801  | W   | Terminate simulator

## Custom UART CSRs

  Name              | CSR    | R/W | Function
  ----------------- | ------ | --- | --------
  `UARTCanPut`      | 0x802  | R   | Can write to UART?
  `UARTPut`         | 0x803  | W   | Write byte to UART
  `UARTCanGet`      | 0x804  | R   | Can read from UART?
  `UARTGet`         | 0x805  | R   | Read byte from UART

## Custom Instruction Memory CSRs

  Name              | CSR    | R/W | Function
  ----------------- | ------ | --- | --------
  `InstrAddr`       | 0x806  | W   | Set instruction mem address
  `WriteInstr`      | 0x807  | W   | Write to instruction mem

## Custom SIMT Host/Management CSRs

  Name                   | CSR    | R/W | Function
  ---------------------- | ------ | --- | --------
  `SIMTCanPut`           | 0x820  | R   | Can issue SIMT request?
  `SIMTInstrAddr`        | 0x821  | W   | Set instruction mem address
  `SIMTWriteInstr`       | 0x822  | W   | Write to instruction mem
  `SIMTStartKernel`      | 0x823  | W   | Start all warps with given PC
  `SIMTCanGet`           | 0x824  | R   | Can get SIMT response?
  `SIMTGet`              | 0x825  | R   | Get SIMT response
  `SIMTSetKernel`        | 0x826  | W   | Set address of kernel closure
  `SIMTSetWarpsPerBlock` | 0x827  | W   | Set num of warps per block

## Custom SIMT Device CSRs

  Name              | CSR    | R/W | Function
  ----------------- | ------ | --- | --------
  `WarpCmd`         | 0x830  | W   | Warp command register
  `WarpGetKernel`   | 0x831  | R   | Get address of kernel closure
