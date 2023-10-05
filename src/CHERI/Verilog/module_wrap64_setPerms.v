//
// Generated by Bluespec Compiler (build 6a8cedf)
//
// On Thu Oct  5 12:21:05 UTC 2023
//
//
// Ports:
// Name                         I/O  size props
// wrap64_setPerms                O    91
// wrap64_setPerms_cap            I    91
// wrap64_setPerms_perms          I    31
//
// Combinational paths from inputs to outputs:
//   (wrap64_setPerms_cap, wrap64_setPerms_perms) -> wrap64_setPerms
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

module module_wrap64_setPerms(wrap64_setPerms_cap,
			      wrap64_setPerms_perms,
			      wrap64_setPerms);
  // value method wrap64_setPerms
  input  [90 : 0] wrap64_setPerms_cap;
  input  [30 : 0] wrap64_setPerms_perms;
  output [90 : 0] wrap64_setPerms;

  // signals for module outputs
  wire [90 : 0] wrap64_setPerms;

  // value method wrap64_setPerms
  assign wrap64_setPerms =
	     { wrap64_setPerms_cap[90:50],
	       wrap64_setPerms_perms[11:0],
	       wrap64_setPerms_cap[37:0] } ;
endmodule  // module_wrap64_setPerms

