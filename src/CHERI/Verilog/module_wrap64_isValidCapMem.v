//
// Generated by Bluespec Compiler (build 6a8cedf)
//
// On Tue Oct 10 19:35:49 UTC 2023
//
//
// Ports:
// Name                         I/O  size props
// wrap64_isValidCapMem           O     1
// wrap64_isValidCapMem_cap       I    65
//
// Combinational paths from inputs to outputs:
//   wrap64_isValidCapMem_cap -> wrap64_isValidCapMem
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

module module_wrap64_isValidCapMem(wrap64_isValidCapMem_cap,
				   wrap64_isValidCapMem);
  // value method wrap64_isValidCapMem
  input  [64 : 0] wrap64_isValidCapMem_cap;
  output wrap64_isValidCapMem;

  // signals for module outputs
  wire wrap64_isValidCapMem;

  // value method wrap64_isValidCapMem
  assign wrap64_isValidCapMem = wrap64_isValidCapMem_cap[64] ;
endmodule  // module_wrap64_isValidCapMem

