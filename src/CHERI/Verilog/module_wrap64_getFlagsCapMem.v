//
// Generated by Bluespec Compiler, version 2022.01-37-gaf852df5 (build af852df5)
//
// On Thu Jun 20 12:37:31 BST 2024
//
//
// Ports:
// Name                         I/O  size props
// wrap64_getFlagsCapMem          O     1
// wrap64_getFlagsCapMem_capMem   I    65
//
// Combinational paths from inputs to outputs:
//   wrap64_getFlagsCapMem_capMem -> wrap64_getFlagsCapMem
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

module module_wrap64_getFlagsCapMem(wrap64_getFlagsCapMem_capMem,
				    wrap64_getFlagsCapMem);
  // value method wrap64_getFlagsCapMem
  input  [64 : 0] wrap64_getFlagsCapMem_capMem;
  output wrap64_getFlagsCapMem;

  // signals for module outputs
  wire wrap64_getFlagsCapMem;

  // value method wrap64_getFlagsCapMem
  assign wrap64_getFlagsCapMem = wrap64_getFlagsCapMem_capMem[51] ;
endmodule  // module_wrap64_getFlagsCapMem
