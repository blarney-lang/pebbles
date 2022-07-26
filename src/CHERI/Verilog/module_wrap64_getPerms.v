//
// Generated by Bluespec Compiler (build 14ff62d)
//
// On Mon Jul 25 15:00:13 BST 2022
//
//
// Ports:
// Name                         I/O  size props
// wrap64_getPerms                O    31
// wrap64_getPerms_cap            I    91
//
// Combinational paths from inputs to outputs:
//   wrap64_getPerms_cap -> wrap64_getPerms
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

module module_wrap64_getPerms(wrap64_getPerms_cap,
			      wrap64_getPerms);
  // value method wrap64_getPerms
  input  [90 : 0] wrap64_getPerms_cap;
  output [30 : 0] wrap64_getPerms;

  // signals for module outputs
  wire [30 : 0] wrap64_getPerms;

  // value method wrap64_getPerms
  assign wrap64_getPerms = { 19'd0, wrap64_getPerms_cap[49:38] } ;
endmodule  // module_wrap64_getPerms

