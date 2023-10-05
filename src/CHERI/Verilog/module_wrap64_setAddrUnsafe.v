//
// Generated by Bluespec Compiler (build 6a8cedf)
//
// On Thu Oct  5 12:21:07 UTC 2023
//
//
// Ports:
// Name                         I/O  size props
// wrap64_setAddrUnsafe           O    91
// wrap64_setAddrUnsafe_cap       I    91
// wrap64_setAddrUnsafe_addr      I    32
//
// Combinational paths from inputs to outputs:
//   (wrap64_setAddrUnsafe_cap,
//    wrap64_setAddrUnsafe_addr) -> wrap64_setAddrUnsafe
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

module module_wrap64_setAddrUnsafe(wrap64_setAddrUnsafe_cap,
				   wrap64_setAddrUnsafe_addr,
				   wrap64_setAddrUnsafe);
  // value method wrap64_setAddrUnsafe
  input  [90 : 0] wrap64_setAddrUnsafe_cap;
  input  [31 : 0] wrap64_setAddrUnsafe_addr;
  output [90 : 0] wrap64_setAddrUnsafe;

  // signals for module outputs
  wire [90 : 0] wrap64_setAddrUnsafe;

  // remaining internal signals
  wire [31 : 0] x__h160;
  wire [4 : 0] wrap64_setAddrUnsafe_addr_SRL_wrap64_setAddrUn_ETC___d23;
  wire [2 : 0] repBound__h315;
  wire wrap64_setAddrUnsafe_addr_SRL_wrap64_setAddrUn_ETC___d13,
       wrap64_setAddrUnsafe_cap_BITS_17_TO_15_ULT_wra_ETC___d11,
       wrap64_setAddrUnsafe_cap_BITS_25_TO_23_ULT_wra_ETC___d10;

  // value method wrap64_setAddrUnsafe
  assign wrap64_setAddrUnsafe =
	     { wrap64_setAddrUnsafe_cap[90],
	       wrap64_setAddrUnsafe_addr,
	       x__h160[7:0],
	       wrap64_setAddrUnsafe_cap[49:10],
	       repBound__h315,
	       wrap64_setAddrUnsafe_cap_BITS_25_TO_23_ULT_wra_ETC___d10,
	       wrap64_setAddrUnsafe_cap_BITS_17_TO_15_ULT_wra_ETC___d11,
	       wrap64_setAddrUnsafe_addr_SRL_wrap64_setAddrUn_ETC___d23 } ;

  // remaining internal signals
  assign repBound__h315 = wrap64_setAddrUnsafe_cap[17:15] - 3'b001 ;
  assign wrap64_setAddrUnsafe_addr_SRL_wrap64_setAddrUn_ETC___d13 =
	     x__h160[7:5] < repBound__h315 ;
  assign wrap64_setAddrUnsafe_addr_SRL_wrap64_setAddrUn_ETC___d23 =
	     { wrap64_setAddrUnsafe_addr_SRL_wrap64_setAddrUn_ETC___d13,
	       (wrap64_setAddrUnsafe_cap_BITS_25_TO_23_ULT_wra_ETC___d10 ==
		wrap64_setAddrUnsafe_addr_SRL_wrap64_setAddrUn_ETC___d13) ?
		 2'd0 :
		 ((wrap64_setAddrUnsafe_cap_BITS_25_TO_23_ULT_wra_ETC___d10 &&
		   !wrap64_setAddrUnsafe_addr_SRL_wrap64_setAddrUn_ETC___d13) ?
		    2'd1 :
		    2'd3),
	       (wrap64_setAddrUnsafe_cap_BITS_17_TO_15_ULT_wra_ETC___d11 ==
		wrap64_setAddrUnsafe_addr_SRL_wrap64_setAddrUn_ETC___d13) ?
		 2'd0 :
		 ((wrap64_setAddrUnsafe_cap_BITS_17_TO_15_ULT_wra_ETC___d11 &&
		   !wrap64_setAddrUnsafe_addr_SRL_wrap64_setAddrUn_ETC___d13) ?
		    2'd1 :
		    2'd3) } ;
  assign wrap64_setAddrUnsafe_cap_BITS_17_TO_15_ULT_wra_ETC___d11 =
	     wrap64_setAddrUnsafe_cap[17:15] < repBound__h315 ;
  assign wrap64_setAddrUnsafe_cap_BITS_25_TO_23_ULT_wra_ETC___d10 =
	     wrap64_setAddrUnsafe_cap[25:23] < repBound__h315 ;
  assign x__h160 =
	     wrap64_setAddrUnsafe_addr >> wrap64_setAddrUnsafe_cap[31:26] ;
endmodule  // module_wrap64_setAddrUnsafe

