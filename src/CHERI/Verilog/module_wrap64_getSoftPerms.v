//
// Generated by Bluespec Compiler (build 14ff62d)
//
// On Thu Oct  7 15:42:58 BST 2021
//
//
// Ports:
// Name                         I/O  size props
// wrap64_getSoftPerms            O    16 const
// wrap64_getSoftPerms_cap        I    91 unused
//
// No combinational paths from inputs to outputs
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

module module_wrap64_getSoftPerms(wrap64_getSoftPerms_cap,
				  wrap64_getSoftPerms);
  // value method wrap64_getSoftPerms
  input  [90 : 0] wrap64_getSoftPerms_cap;
  output [15 : 0] wrap64_getSoftPerms;

  // signals for module outputs
  wire [15 : 0] wrap64_getSoftPerms;

  // value method wrap64_getSoftPerms
  assign wrap64_getSoftPerms = 16'd0 ;
endmodule  // module_wrap64_getSoftPerms

