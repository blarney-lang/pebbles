//
// Generated by Bluespec Compiler (build 14ff62d)
//
// On Mon Jul 25 15:00:14 BST 2022
//
//
// Ports:
// Name                         I/O  size props
// wrap64_fromMem                 O    91
// wrap64_fromMem_mem_cap         I    65
//
// Combinational paths from inputs to outputs:
//   wrap64_fromMem_mem_cap -> wrap64_fromMem
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

module module_wrap64_fromMem(wrap64_fromMem_mem_cap,
			     wrap64_fromMem);
  // value method wrap64_fromMem
  input  [64 : 0] wrap64_fromMem_mem_cap;
  output [90 : 0] wrap64_fromMem;

  // signals for module outputs
  wire [90 : 0] wrap64_fromMem;

  // remaining internal signals
  wire [31 : 0] x__h394;
  wire [26 : 0] INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_CONCA_ETC___d42;
  wire [7 : 0] b_base__h619, res_addrBits__h126, x__h592, x__h612;
  wire [5 : 0] b_top__h618, topBits__h521, x__h432;
  wire [4 : 0] IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d61,
	       INV_wrap64_fromMem_mem_cap_BITS_50_TO_46__q1;
  wire [2 : 0] repBound__h673,
	       tb__h670,
	       tmp_expBotHalf__h387,
	       tmp_expTopHalf__h385;
  wire [1 : 0] carry_out__h523,
	       impliedTopBits__h525,
	       len_correction__h524,
	       x__h609;
  wire IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d48,
       IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d49,
       IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d51;

  // value method wrap64_fromMem
  assign wrap64_fromMem =
	     { wrap64_fromMem_mem_cap[64],
	       wrap64_fromMem_mem_cap[31:0],
	       res_addrBits__h126,
	       wrap64_fromMem_mem_cap[63:51],
	       INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_CONCA_ETC___d42,
	       repBound__h673,
	       IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d48,
	       IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d49,
	       IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d61 } ;

  // remaining internal signals
  assign IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d48 =
	     tb__h670 < repBound__h673 ;
  assign IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d49 =
	     x__h612[7:5] < repBound__h673 ;
  assign IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d51 =
	     res_addrBits__h126[7:5] < repBound__h673 ;
  assign IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d61 =
	     { IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d51,
	       (IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d48 ==
		IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d51) ?
		 2'd0 :
		 ((IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d48 &&
		   !IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d51) ?
		    2'd1 :
		    2'd3),
	       (IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d49 ==
		IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d51) ?
		 2'd0 :
		 ((IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d49 &&
		   !IF_INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_BI_ETC___d51) ?
		    2'd1 :
		    2'd3) } ;
  assign INV_wrap64_fromMem_mem_cap_BITS_50_TO_46_CONCA_ETC___d42 =
	     { ~wrap64_fromMem_mem_cap[50:46],
	       INV_wrap64_fromMem_mem_cap_BITS_50_TO_46__q1[0] ?
		 x__h432 :
		 6'd0,
	       x__h592,
	       x__h612 } ;
  assign INV_wrap64_fromMem_mem_cap_BITS_50_TO_46__q1 =
	     ~wrap64_fromMem_mem_cap[50:46] ;
  assign b_base__h619 =
	     { wrap64_fromMem_mem_cap[39:34],
	       ~wrap64_fromMem_mem_cap[33],
	       wrap64_fromMem_mem_cap[32] } ;
  assign b_top__h618 =
	     { wrap64_fromMem_mem_cap[45:42],
	       ~wrap64_fromMem_mem_cap[41:40] } ;
  assign carry_out__h523 = (topBits__h521 < x__h612[5:0]) ? 2'b01 : 2'b0 ;
  assign impliedTopBits__h525 = x__h609 + len_correction__h524 ;
  assign len_correction__h524 =
	     INV_wrap64_fromMem_mem_cap_BITS_50_TO_46__q1[0] ? 2'b01 : 2'b0 ;
  assign repBound__h673 = x__h612[7:5] - 3'b001 ;
  assign res_addrBits__h126 =
	     INV_wrap64_fromMem_mem_cap_BITS_50_TO_46__q1[0] ?
	       x__h394[7:0] :
	       wrap64_fromMem_mem_cap[7:0] ;
  assign tb__h670 = { impliedTopBits__h525, topBits__h521[5] } ;
  assign tmp_expBotHalf__h387 =
	     { wrap64_fromMem_mem_cap[34],
	       ~wrap64_fromMem_mem_cap[33],
	       wrap64_fromMem_mem_cap[32] } ;
  assign tmp_expTopHalf__h385 =
	     { wrap64_fromMem_mem_cap[42], ~wrap64_fromMem_mem_cap[41:40] } ;
  assign topBits__h521 =
	     INV_wrap64_fromMem_mem_cap_BITS_50_TO_46__q1[0] ?
	       { wrap64_fromMem_mem_cap[45:43], 3'd0 } :
	       b_top__h618 ;
  assign x__h394 = wrap64_fromMem_mem_cap[31:0] >> x__h432 ;
  assign x__h432 = { tmp_expTopHalf__h385, tmp_expBotHalf__h387 } ;
  assign x__h592 = { impliedTopBits__h525, topBits__h521 } ;
  assign x__h609 = x__h612[7:6] + carry_out__h523 ;
  assign x__h612 =
	     INV_wrap64_fromMem_mem_cap_BITS_50_TO_46__q1[0] ?
	       { wrap64_fromMem_mem_cap[39:35], 3'd0 } :
	       b_base__h619 ;
endmodule  // module_wrap64_fromMem

