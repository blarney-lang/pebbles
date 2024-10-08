//
// Generated by Bluespec Compiler, version 2022.01-37-gaf852df5 (build af852df5)
//
// On Fri Jul  5 14:25:22 BST 2024
//
//
// Ports:
// Name                         I/O  size props
// wrap64_setFlags                O    91
// wrap64_setFlags_cap            I    91
// wrap64_setFlags_flags          I     1
//
// Combinational paths from inputs to outputs:
//   (wrap64_setFlags_cap, wrap64_setFlags_flags) -> wrap64_setFlags
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

module module_wrap64_setFlags(wrap64_setFlags_cap,
			      wrap64_setFlags_flags,
			      wrap64_setFlags);
  // value method wrap64_setFlags
  input  [90 : 0] wrap64_setFlags_cap;
  input  wrap64_setFlags_flags;
  output [90 : 0] wrap64_setFlags;

  // signals for module outputs
  wire [90 : 0] wrap64_setFlags;

  // value method wrap64_setFlags
  assign wrap64_setFlags =
	     { wrap64_setFlags_cap[90:38],
	       wrap64_setFlags_flags,
	       wrap64_setFlags_cap[36:0] } ;
endmodule  // module_wrap64_setFlags

