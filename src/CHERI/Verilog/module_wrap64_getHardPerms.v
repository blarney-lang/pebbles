//
// Generated by Bluespec Compiler (build 14ff62d)
//
// On Thu Oct  7 15:42:58 BST 2021
//
//
// Ports:
// Name                         I/O  size props
// wrap64_getHardPerms            O    12
// wrap64_getHardPerms_cap        I    91
//
// Combinational paths from inputs to outputs:
//   wrap64_getHardPerms_cap -> wrap64_getHardPerms
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

module module_wrap64_getHardPerms(wrap64_getHardPerms_cap,
				  wrap64_getHardPerms);
  // value method wrap64_getHardPerms
  input  [90 : 0] wrap64_getHardPerms_cap;
  output [11 : 0] wrap64_getHardPerms;

  // signals for module outputs
  wire [11 : 0] wrap64_getHardPerms;

  // value method wrap64_getHardPerms
  assign wrap64_getHardPerms = wrap64_getHardPerms_cap[49:38] ;
endmodule  // module_wrap64_getHardPerms

