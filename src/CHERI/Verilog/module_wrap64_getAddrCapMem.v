//
// Generated by Bluespec Compiler, version 2022.01-37-gaf852df5 (build af852df5)
//
// On Fri Jul  5 14:25:24 BST 2024
//
//
// Ports:
// Name                         I/O  size props
// wrap64_getAddrCapMem           O    32
// wrap64_getAddrCapMem_cap       I    65
//
// Combinational paths from inputs to outputs:
//   wrap64_getAddrCapMem_cap -> wrap64_getAddrCapMem
//
//

`ifdef BSV_ASSIGNMENT_DELAY
`else
  `define BSV_ASSIGNMENT_DELAY
`endif

`ifdef BSV_POSITIVE_RESET
  `define BSV_RESET_VALUE 1'b1
  `define BSV_RESET_EDGE posedge
`else
  `define BSV_RESET_VALUE 1'b0
  `define BSV_RESET_EDGE negedge
`endif

module module_wrap64_getAddrCapMem(wrap64_getAddrCapMem_cap,
				   wrap64_getAddrCapMem);
  // value method wrap64_getAddrCapMem
  input  [64 : 0] wrap64_getAddrCapMem_cap;
  output [31 : 0] wrap64_getAddrCapMem;

  // signals for module outputs
  wire [31 : 0] wrap64_getAddrCapMem;

  // value method wrap64_getAddrCapMem
  assign wrap64_getAddrCapMem = wrap64_getAddrCapMem_cap[31:0] ;
endmodule  // module_wrap64_getAddrCapMem

