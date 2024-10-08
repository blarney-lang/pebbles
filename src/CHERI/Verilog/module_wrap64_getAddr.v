//
// Generated by Bluespec Compiler, version 2022.01-37-gaf852df5 (build af852df5)
//
// On Fri Jul  5 14:25:22 BST 2024
//
//
// Ports:
// Name                         I/O  size props
// wrap64_getAddr                 O    32
// wrap64_getAddr_cap             I    91
//
// Combinational paths from inputs to outputs:
//   wrap64_getAddr_cap -> wrap64_getAddr
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

module module_wrap64_getAddr(wrap64_getAddr_cap,
			     wrap64_getAddr);
  // value method wrap64_getAddr
  input  [90 : 0] wrap64_getAddr_cap;
  output [31 : 0] wrap64_getAddr;

  // signals for module outputs
  wire [31 : 0] wrap64_getAddr;

  // value method wrap64_getAddr
  assign wrap64_getAddr = wrap64_getAddr_cap[89:58] ;
endmodule  // module_wrap64_getAddr

