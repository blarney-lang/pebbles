//
// Generated by Bluespec Compiler (build 14ff62d)
//
// On Mon Jul 25 15:00:13 BST 2022
//
//
// Ports:
// Name                         I/O  size props
// wrap64_setHardPerms            O    91
// wrap64_setHardPerms_cap        I    91
// wrap64_setHardPerms_hardperms  I    12
//
// Combinational paths from inputs to outputs:
//   (wrap64_setHardPerms_cap,
//    wrap64_setHardPerms_hardperms) -> wrap64_setHardPerms
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

module module_wrap64_setHardPerms(wrap64_setHardPerms_cap,
				  wrap64_setHardPerms_hardperms,
				  wrap64_setHardPerms);
  // value method wrap64_setHardPerms
  input  [90 : 0] wrap64_setHardPerms_cap;
  input  [11 : 0] wrap64_setHardPerms_hardperms;
  output [90 : 0] wrap64_setHardPerms;

  // signals for module outputs
  wire [90 : 0] wrap64_setHardPerms;

  // value method wrap64_setHardPerms
  assign wrap64_setHardPerms =
	     { wrap64_setHardPerms_cap[90:50],
	       wrap64_setHardPerms_hardperms,
	       wrap64_setHardPerms_cap[37:0] } ;
endmodule  // module_wrap64_setHardPerms

