//
// Generated by Bluespec Compiler (build 14ff62d)
//
// On Mon Jul 25 15:00:13 BST 2022
//
//
// Ports:
// Name                         I/O  size props
// wrap64_getLength               O    33
// wrap64_getLength_cap           I    91
//
// Combinational paths from inputs to outputs:
//   wrap64_getLength_cap -> wrap64_getLength
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

module module_wrap64_getLength(wrap64_getLength_cap,
			       wrap64_getLength);
  // value method wrap64_getLength
  input  [90 : 0] wrap64_getLength_cap;
  output [32 : 0] wrap64_getLength;

  // signals for module outputs
  wire [32 : 0] wrap64_getLength;

  // remaining internal signals
  wire [32 : 0] length__h64;
  wire [9 : 0] base__h63, top__h62, x__h119;

  // value method wrap64_getLength
  assign wrap64_getLength =
	     (wrap64_getLength_cap[31:26] < 6'd26) ?
	       length__h64 :
	       33'h1FFFFFFFF ;

  // remaining internal signals
  assign base__h63 =
	     { wrap64_getLength_cap[1:0], wrap64_getLength_cap[17:10] } ;
  assign length__h64 = { 23'd0, x__h119 } << wrap64_getLength_cap[31:26] ;
  assign top__h62 =
	     { wrap64_getLength_cap[3:2], wrap64_getLength_cap[25:18] } ;
  assign x__h119 = top__h62 - base__h63 ;
endmodule  // module_wrap64_getLength

