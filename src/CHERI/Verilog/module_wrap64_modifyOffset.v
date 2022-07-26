//
// Generated by Bluespec Compiler (build 14ff62d)
//
// On Mon Jul 25 15:00:13 BST 2022
//
//
// Ports:
// Name                         I/O  size props
// wrap64_modifyOffset            O    92
// wrap64_modifyOffset_cap        I    91
// wrap64_modifyOffset_offset     I    32
// wrap64_modifyOffset_doInc      I     1
//
// Combinational paths from inputs to outputs:
//   (wrap64_modifyOffset_cap,
//    wrap64_modifyOffset_offset,
//    wrap64_modifyOffset_doInc) -> wrap64_modifyOffset
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

module module_wrap64_modifyOffset(wrap64_modifyOffset_cap,
				  wrap64_modifyOffset_offset,
				  wrap64_modifyOffset_doInc,
				  wrap64_modifyOffset);
  // value method wrap64_modifyOffset
  input  [90 : 0] wrap64_modifyOffset_cap;
  input  [31 : 0] wrap64_modifyOffset_offset;
  input  wrap64_modifyOffset_doInc;
  output [91 : 0] wrap64_modifyOffset;

  // signals for module outputs
  wire [91 : 0] wrap64_modifyOffset;

  // remaining internal signals
  reg [1 : 0] mask__h579;
  wire [31 : 0] addBase__h621,
		pointer__h72,
		result_d_address__h593,
		ret___1_address__h603,
		x__h485,
		x__h731;
  wire [23 : 0] highOffsetBits__h79, mask__h622, signBits__h76, x__h106;
  wire [9 : 0] x__h672;
  wire [7 : 0] newAddrBits__h578,
	       repBoundBits__h85,
	       result_d_addrBits__h594,
	       toBoundsM1_A__h84,
	       toBoundsM1_B__h87,
	       toBoundsM1__h89,
	       toBounds_A__h83,
	       toBounds_B__h86,
	       toBounds__h88;
  wire [3 : 0] IF_wrap64_modifyOffset_cap_BITS_25_TO_23_9_ULT_ETC___d83;
  wire [2 : 0] repBound__h870;
  wire IF_wrap64_modifyOffset_doInc_THEN_wrap64_modif_ETC___d73,
       IF_wrap64_modifyOffset_offset_BIT_31_THEN_NOT__ETC___d34,
       NOT_wrap64_modifyOffset_offset_SRL_wrap64_modi_ETC___d25,
       wrap64_modifyOffset_cap_BITS_17_TO_15_7_ULT_wr_ETC___d71,
       wrap64_modifyOffset_cap_BITS_25_TO_23_9_ULT_wr_ETC___d70;

  // value method wrap64_modifyOffset
  assign wrap64_modifyOffset =
	     { highOffsetBits__h79 == 24'd0 &&
	       IF_wrap64_modifyOffset_offset_BIT_31_THEN_NOT__ETC___d34 ||
	       wrap64_modifyOffset_cap[31:26] >= 6'd24,
	       (highOffsetBits__h79 == 24'd0 &&
		IF_wrap64_modifyOffset_offset_BIT_31_THEN_NOT__ETC___d34 ||
		wrap64_modifyOffset_cap[31:26] >= 6'd24) &&
	       wrap64_modifyOffset_cap[90],
	       result_d_address__h593,
	       result_d_addrBits__h594,
	       wrap64_modifyOffset_cap[49:10],
	       repBound__h870,
	       wrap64_modifyOffset_cap_BITS_25_TO_23_9_ULT_wr_ETC___d70,
	       wrap64_modifyOffset_cap_BITS_17_TO_15_7_ULT_wr_ETC___d71,
	       IF_wrap64_modifyOffset_doInc_THEN_wrap64_modif_ETC___d73,
	       IF_wrap64_modifyOffset_cap_BITS_25_TO_23_9_ULT_ETC___d83 } ;

  // remaining internal signals
  assign IF_wrap64_modifyOffset_cap_BITS_25_TO_23_9_ULT_ETC___d83 =
	     { (wrap64_modifyOffset_cap_BITS_25_TO_23_9_ULT_wr_ETC___d70 ==
		IF_wrap64_modifyOffset_doInc_THEN_wrap64_modif_ETC___d73) ?
		 2'd0 :
		 ((wrap64_modifyOffset_cap_BITS_25_TO_23_9_ULT_wr_ETC___d70 &&
		   !IF_wrap64_modifyOffset_doInc_THEN_wrap64_modif_ETC___d73) ?
		    2'd1 :
		    2'd3),
	       (wrap64_modifyOffset_cap_BITS_17_TO_15_7_ULT_wr_ETC___d71 ==
		IF_wrap64_modifyOffset_doInc_THEN_wrap64_modif_ETC___d73) ?
		 2'd0 :
		 ((wrap64_modifyOffset_cap_BITS_17_TO_15_7_ULT_wr_ETC___d71 &&
		   !IF_wrap64_modifyOffset_doInc_THEN_wrap64_modif_ETC___d73) ?
		    2'd1 :
		    2'd3) } ;
  assign IF_wrap64_modifyOffset_doInc_THEN_wrap64_modif_ETC___d73 =
	     result_d_addrBits__h594[7:5] < repBound__h870 ;
  assign IF_wrap64_modifyOffset_offset_BIT_31_THEN_NOT__ETC___d34 =
	     wrap64_modifyOffset_offset[31] ?
	       NOT_wrap64_modifyOffset_offset_SRL_wrap64_modi_ETC___d25 :
	       (wrap64_modifyOffset_doInc ?
		  x__h485[7:0] < toBoundsM1__h89 :
		  x__h485[7:0] <= toBoundsM1__h89) ;
  assign NOT_wrap64_modifyOffset_offset_SRL_wrap64_modi_ETC___d25 =
	     x__h485[7:0] >= toBounds__h88 &&
	     (!wrap64_modifyOffset_doInc ||
	      repBoundBits__h85 != wrap64_modifyOffset_cap[57:50]) ;
  assign addBase__h621 =
	     { {22{x__h672[9]}}, x__h672 } << wrap64_modifyOffset_cap[31:26] ;
  assign highOffsetBits__h79 = x__h106 & mask__h622 ;
  assign mask__h622 = 24'd16777215 << wrap64_modifyOffset_cap[31:26] ;
  assign newAddrBits__h578 = wrap64_modifyOffset_cap[17:10] + x__h485[7:0] ;
  assign pointer__h72 =
	     wrap64_modifyOffset_cap[89:58] + wrap64_modifyOffset_offset ;
  assign repBoundBits__h85 = { wrap64_modifyOffset_cap[9:7], 5'd0 } ;
  assign repBound__h870 = wrap64_modifyOffset_cap[17:15] - 3'b001 ;
  assign result_d_addrBits__h594 =
	     wrap64_modifyOffset_doInc ?
	       x__h731[7:0] :
	       { mask__h579, 6'd63 } & newAddrBits__h578 ;
  assign result_d_address__h593 =
	     wrap64_modifyOffset_doInc ?
	       pointer__h72 :
	       ret___1_address__h603 ;
  assign ret___1_address__h603 =
	     { wrap64_modifyOffset_cap[89:66] & mask__h622, 8'd0 } +
	     addBase__h621 +
	     wrap64_modifyOffset_offset ;
  assign signBits__h76 = {24{wrap64_modifyOffset_offset[31]}} ;
  assign toBoundsM1_A__h84 = { 3'b110, ~wrap64_modifyOffset_cap[14:10] } ;
  assign toBoundsM1_B__h87 =
	     repBoundBits__h85 + ~wrap64_modifyOffset_cap[57:50] ;
  assign toBoundsM1__h89 =
	     wrap64_modifyOffset_doInc ?
	       toBoundsM1_B__h87 :
	       toBoundsM1_A__h84 ;
  assign toBounds_A__h83 = 8'd224 - { 3'b0, wrap64_modifyOffset_cap[14:10] } ;
  assign toBounds_B__h86 =
	     repBoundBits__h85 - wrap64_modifyOffset_cap[57:50] ;
  assign toBounds__h88 =
	     wrap64_modifyOffset_doInc ? toBounds_B__h86 : toBounds_A__h83 ;
  assign wrap64_modifyOffset_cap_BITS_17_TO_15_7_ULT_wr_ETC___d71 =
	     wrap64_modifyOffset_cap[17:15] < repBound__h870 ;
  assign wrap64_modifyOffset_cap_BITS_25_TO_23_9_ULT_wr_ETC___d70 =
	     wrap64_modifyOffset_cap[25:23] < repBound__h870 ;
  assign x__h106 = wrap64_modifyOffset_offset[31:8] ^ signBits__h76 ;
  assign x__h485 =
	     wrap64_modifyOffset_offset >> wrap64_modifyOffset_cap[31:26] ;
  assign x__h672 =
	     { wrap64_modifyOffset_cap[1:0],
	       wrap64_modifyOffset_cap[17:10] } ;
  assign x__h731 = pointer__h72 >> wrap64_modifyOffset_cap[31:26] ;
  always@(wrap64_modifyOffset_cap)
  begin
    case (wrap64_modifyOffset_cap[31:26])
      6'd25: mask__h579 = 2'b01;
      6'd26: mask__h579 = 2'b0;
      default: mask__h579 = 2'b11;
    endcase
  end
endmodule  // module_wrap64_modifyOffset

