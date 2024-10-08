//
// Generated by Bluespec Compiler, version 2022.01-37-gaf852df5 (build af852df5)
//
// On Fri Jul  5 14:25:22 BST 2024
//
//
// Ports:
// Name                         I/O  size props
// wrap64_isSealedWithType        O     1
// wrap64_isSealedWithType_cap    I    91
//
// Combinational paths from inputs to outputs:
//   wrap64_isSealedWithType_cap -> wrap64_isSealedWithType
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

module module_wrap64_isSealedWithType(wrap64_isSealedWithType_cap,
				      wrap64_isSealedWithType);
  // value method wrap64_isSealedWithType
  input  [90 : 0] wrap64_isSealedWithType_cap;
  output wrap64_isSealedWithType;

  // signals for module outputs
  wire wrap64_isSealedWithType;

  // value method wrap64_isSealedWithType
  assign wrap64_isSealedWithType =
	     wrap64_isSealedWithType_cap[36:33] != 4'd15 &&
	     wrap64_isSealedWithType_cap[36:33] != 4'd14 &&
	     wrap64_isSealedWithType_cap[36:33] != 4'd13 &&
	     wrap64_isSealedWithType_cap[36:33] != 4'd12 ;
endmodule  // module_wrap64_isSealedWithType

