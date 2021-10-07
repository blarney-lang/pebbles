//
// Generated by Bluespec Compiler (build 14ff62d)
//
// On Thu Oct  7 15:42:58 BST 2021
//
//
// Ports:
// Name                         I/O  size props
// wrap64_toMem                   O    65
// wrap64_toMem_cap               I    91
//
// Combinational paths from inputs to outputs:
//   wrap64_toMem_cap -> wrap64_toMem
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

module module_wrap64_toMem(wrap64_toMem_cap,
			   wrap64_toMem);
  // value method wrap64_toMem
  input  [90 : 0] wrap64_toMem_cap;
  output [64 : 0] wrap64_toMem;

  // signals for module outputs
  wire [64 : 0] wrap64_toMem;

  // remaining internal signals
  wire [63 : 0] x__h448;
  wire [13 : 0] IF_wrap64_toMem_cap_BIT_32_THEN_wrap64_toMem_c_ETC___d13;

  // value method wrap64_toMem
  assign wrap64_toMem = { wrap64_toMem_cap[90], x__h448 } ;

  // remaining internal signals
  assign IF_wrap64_toMem_cap_BIT_32_THEN_wrap64_toMem_c_ETC___d13 =
	     wrap64_toMem_cap[32] ?
	       { wrap64_toMem_cap[23:21],
		 wrap64_toMem_cap[31:29],
		 wrap64_toMem_cap[17:13],
		 wrap64_toMem_cap[28:26] } :
	       wrap64_toMem_cap[23:10] ;
  assign x__h448 =
	     { wrap64_toMem_cap[49:37],
	       ~wrap64_toMem_cap[36:32],
	       IF_wrap64_toMem_cap_BIT_32_THEN_wrap64_toMem_c_ETC___d13[13:10],
	       ~IF_wrap64_toMem_cap_BIT_32_THEN_wrap64_toMem_c_ETC___d13[9:8],
	       IF_wrap64_toMem_cap_BIT_32_THEN_wrap64_toMem_c_ETC___d13[7:2],
	       ~IF_wrap64_toMem_cap_BIT_32_THEN_wrap64_toMem_c_ETC___d13[1],
	       IF_wrap64_toMem_cap_BIT_32_THEN_wrap64_toMem_c_ETC___d13[0],
	       wrap64_toMem_cap[89:58] } ;
endmodule  // module_wrap64_toMem

