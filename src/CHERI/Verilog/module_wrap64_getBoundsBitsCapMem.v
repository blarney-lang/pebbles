//
// Generated by Bluespec Compiler, version 2022.01-37-gaf852df5 (build af852df5)
//
// On Fri Jul  5 14:25:24 BST 2024
//
//
// Ports:
// Name                         I/O  size props
// wrap64_getBoundsBitsCapMem     O    15
// wrap64_getBoundsBitsCapMem_capMem  I    65
//
// Combinational paths from inputs to outputs:
//   wrap64_getBoundsBitsCapMem_capMem -> wrap64_getBoundsBitsCapMem
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

module module_wrap64_getBoundsBitsCapMem(wrap64_getBoundsBitsCapMem_capMem,
					 wrap64_getBoundsBitsCapMem);
  // value method wrap64_getBoundsBitsCapMem
  input  [64 : 0] wrap64_getBoundsBitsCapMem_capMem;
  output [14 : 0] wrap64_getBoundsBitsCapMem;

  // signals for module outputs
  wire [14 : 0] wrap64_getBoundsBitsCapMem;

  // value method wrap64_getBoundsBitsCapMem
  assign wrap64_getBoundsBitsCapMem =
	     wrap64_getBoundsBitsCapMem_capMem[46:32] ;
endmodule  // module_wrap64_getBoundsBitsCapMem

