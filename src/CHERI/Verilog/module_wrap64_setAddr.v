//
// Generated by Bluespec Compiler (build 14ff62d)
//
// On Thu Oct  7 15:42:58 BST 2021
//
//
// Ports:
// Name                         I/O  size props
// wrap64_setAddr                 O    92
// wrap64_setAddr_cap             I    91
// wrap64_setAddr_addr            I    32
//
// Combinational paths from inputs to outputs:
//   (wrap64_setAddr_cap, wrap64_setAddr_addr) -> wrap64_setAddr
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

module module_wrap64_setAddr(wrap64_setAddr_cap,
			     wrap64_setAddr_addr,
			     wrap64_setAddr);
  // value method wrap64_setAddr
  input  [90 : 0] wrap64_setAddr_cap;
  input  [31 : 0] wrap64_setAddr_addr;
  output [91 : 0] wrap64_setAddr;

  // signals for module outputs
  wire [91 : 0] wrap64_setAddr;

  // remaining internal signals
  wire [31 : 0] x__h99;
  wire [23 : 0] deltaAddrHi__h74, deltaAddrUpper__h76, mask__h75;
  wire [3 : 0] IF_wrap64_setAddr_cap_BITS_25_TO_23_7_ULT_wrap_ETC___d40;
  wire [2 : 0] repBound__h420;
  wire [1 : 0] x__h82;
  wire SEXT__0b0_CONCAT_wrap64_setAddr_addr_SRL_wrap6_ETC___d18,
       wrap64_setAddr_addr_SRL_wrap64_setAddr_cap_BIT_ETC___d30,
       wrap64_setAddr_cap_BITS_17_TO_15_5_ULT_wrap64__ETC___d29,
       wrap64_setAddr_cap_BITS_25_TO_23_7_ULT_wrap64__ETC___d28;

  // value method wrap64_setAddr
  assign wrap64_setAddr =
	     { SEXT__0b0_CONCAT_wrap64_setAddr_addr_SRL_wrap6_ETC___d18,
	       SEXT__0b0_CONCAT_wrap64_setAddr_addr_SRL_wrap6_ETC___d18 &&
	       wrap64_setAddr_cap[90],
	       wrap64_setAddr_addr,
	       x__h99[7:0],
	       wrap64_setAddr_cap[49:10],
	       repBound__h420,
	       wrap64_setAddr_cap_BITS_25_TO_23_7_ULT_wrap64__ETC___d28,
	       wrap64_setAddr_cap_BITS_17_TO_15_5_ULT_wrap64__ETC___d29,
	       wrap64_setAddr_addr_SRL_wrap64_setAddr_cap_BIT_ETC___d30,
	       IF_wrap64_setAddr_cap_BITS_25_TO_23_7_ULT_wrap_ETC___d40 } ;

  // remaining internal signals
  assign IF_wrap64_setAddr_cap_BITS_25_TO_23_7_ULT_wrap_ETC___d40 =
	     { (wrap64_setAddr_cap_BITS_25_TO_23_7_ULT_wrap64__ETC___d28 ==
		wrap64_setAddr_addr_SRL_wrap64_setAddr_cap_BIT_ETC___d30) ?
		 2'd0 :
		 ((wrap64_setAddr_cap_BITS_25_TO_23_7_ULT_wrap64__ETC___d28 &&
		   !wrap64_setAddr_addr_SRL_wrap64_setAddr_cap_BIT_ETC___d30) ?
		    2'd1 :
		    2'd3),
	       (wrap64_setAddr_cap_BITS_17_TO_15_5_ULT_wrap64__ETC___d29 ==
		wrap64_setAddr_addr_SRL_wrap64_setAddr_cap_BIT_ETC___d30) ?
		 2'd0 :
		 ((wrap64_setAddr_cap_BITS_17_TO_15_5_ULT_wrap64__ETC___d29 &&
		   !wrap64_setAddr_addr_SRL_wrap64_setAddr_cap_BIT_ETC___d30) ?
		    2'd1 :
		    2'd3) } ;
  assign SEXT__0b0_CONCAT_wrap64_setAddr_addr_SRL_wrap6_ETC___d18 =
	     deltaAddrHi__h74 == deltaAddrUpper__h76 ;
  assign deltaAddrHi__h74 =
	     { {22{x__h82[1]}}, x__h82 } << wrap64_setAddr_cap[31:26] ;
  assign deltaAddrUpper__h76 =
	     (wrap64_setAddr_addr[31:8] & mask__h75) -
	     (wrap64_setAddr_cap[89:66] & mask__h75) ;
  assign mask__h75 = 24'd16777215 << wrap64_setAddr_cap[31:26] ;
  assign repBound__h420 = wrap64_setAddr_cap[17:15] - 3'b001 ;
  assign wrap64_setAddr_addr_SRL_wrap64_setAddr_cap_BIT_ETC___d30 =
	     x__h99[7:5] < repBound__h420 ;
  assign wrap64_setAddr_cap_BITS_17_TO_15_5_ULT_wrap64__ETC___d29 =
	     wrap64_setAddr_cap[17:15] < repBound__h420 ;
  assign wrap64_setAddr_cap_BITS_25_TO_23_7_ULT_wrap64__ETC___d28 =
	     wrap64_setAddr_cap[25:23] < repBound__h420 ;
  assign x__h82 =
	     { 1'b0, x__h99[7:5] < wrap64_setAddr_cap[9:7] } -
	     { 1'b0, wrap64_setAddr_cap[4] } ;
  assign x__h99 = wrap64_setAddr_addr >> wrap64_setAddr_cap[31:26] ;
endmodule  // module_wrap64_setAddr

