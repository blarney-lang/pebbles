//
// Generated by Bluespec Compiler, version 2022.01-37-gaf852df5 (build af852df5)
//
// On Fri Jul  5 14:25:23 BST 2024
//
//
// Ports:
// Name                         I/O  size props
// wrap64_setBoundsCombined       O   156
// wrap64_setBoundsCombined_cap   I    91
// wrap64_setBoundsCombined_length  I    32
//
// Combinational paths from inputs to outputs:
//   (wrap64_setBoundsCombined_cap,
//    wrap64_setBoundsCombined_length) -> wrap64_setBoundsCombined
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

module module_wrap64_setBoundsCombined(wrap64_setBoundsCombined_cap,
				       wrap64_setBoundsCombined_length,
				       wrap64_setBoundsCombined);
  // value method wrap64_setBoundsCombined
  input  [90 : 0] wrap64_setBoundsCombined_cap;
  input  [31 : 0] wrap64_setBoundsCombined_length;
  output [155 : 0] wrap64_setBoundsCombined;

  // signals for module outputs
  wire [155 : 0] wrap64_setBoundsCombined;

  // remaining internal signals
  wire [33 : 0] _theResult_____1_fst__h4965,
		baseMask___1__h4964,
		base__h132,
		lmaskLo__h140,
		lmaskLor__h139,
		mwLsbMask__h148,
		newLengthRounded__h4961,
		newLengthRounded__h4963,
		newLength___2__h4962,
		newLength__h158,
		oneInLsb__h4960,
		top__h135,
		x__h4503,
		x__h4632,
		x__h4811,
		x__h4954,
		x__h5021,
		y__h4504,
		y__h4982;
  wire [31 : 0] wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d20,
		wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d10,
		wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d13,
		wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d16,
		wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d4,
		wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d7,
		wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC__q1;
  wire [15 : 0] IF_NOT_wrap64_setBoundsCombined_length_BIT_31__ETC___d207;
  wire [8 : 0] x__h4850;
  wire [7 : 0] _theResult_____3_fst_bounds_topBits__h4621,
	       result_cap_addrBits__h4278,
	       ret_bounds_baseBits__h4793,
	       ret_bounds_topBits__h4617,
	       ret_bounds_topBits__h4842,
	       x__h4886,
	       x__h4889;
  wire [5 : 0] IF_wrap64_setBoundsCombined_length_AND_15_CONC_ETC___d134,
	       _25_MINUS_0_CONCAT_IF_wrap64_setBoundsCombined__ETC___d127;
  wire [3 : 0] IF_IF_NOT_wrap64_setBoundsCombined_length_BIT__ETC___d229;
  wire [2 : 0] repBound__h4879;
  wire IF_NOT_wrap64_setBoundsCombined_length_BIT_31__ETC___d216,
       IF_NOT_wrap64_setBoundsCombined_length_BIT_31__ETC___d217,
       IF_wrap64_setBoundsCombined_length_AND_15_CONC_ETC___d219,
       NOT_0_CONCAT_wrap64_setBoundsCombined_length_O_ETC___d37,
       NOT_0b0_CONCAT_wrap64_setBoundsCombined_cap_BI_ETC___d186,
       NOT_0b0_CONCAT_wrap64_setBoundsCombined_cap_BI_ETC___d197,
       _0b0_CONCAT_wrap64_setBoundsCombined_cap_BITS_8_ETC___d235,
       wrap64_setBoundsCombined_cap_BITS_89_TO_58_8_A_ETC___d234,
       wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d38,
       wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d45,
       wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d47,
       wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d48;

  // value method wrap64_setBoundsCombined
  assign wrap64_setBoundsCombined =
	     { wrap64_setBoundsCombined_cap[90:58],
	       result_cap_addrBits__h4278,
	       wrap64_setBoundsCombined_cap[49:37],
	       4'd15,
	       wrap64_setBoundsCombined_length[31] ||
	       wrap64_setBoundsCombined_length[30] ||
	       wrap64_setBoundsCombined_length[29] ||
	       wrap64_setBoundsCombined_length[28] ||
	       wrap64_setBoundsCombined_length[27] ||
	       wrap64_setBoundsCombined_length[26] ||
	       wrap64_setBoundsCombined_length[25] ||
	       wrap64_setBoundsCombined_length[24] ||
	       wrap64_setBoundsCombined_length[23] ||
	       wrap64_setBoundsCombined_length[22] ||
	       wrap64_setBoundsCombined_length[21] ||
	       wrap64_setBoundsCombined_length[20] ||
	       wrap64_setBoundsCombined_length[19] ||
	       wrap64_setBoundsCombined_length[18] ||
	       wrap64_setBoundsCombined_length[17] ||
	       wrap64_setBoundsCombined_length[16] ||
	       wrap64_setBoundsCombined_length[15] ||
	       wrap64_setBoundsCombined_length[14] ||
	       wrap64_setBoundsCombined_length[13] ||
	       wrap64_setBoundsCombined_length[12] ||
	       wrap64_setBoundsCombined_length[11] ||
	       wrap64_setBoundsCombined_length[10] ||
	       wrap64_setBoundsCombined_length[9] ||
	       wrap64_setBoundsCombined_length[8] ||
	       wrap64_setBoundsCombined_length[7] ||
	       wrap64_setBoundsCombined_length[6],
	       IF_wrap64_setBoundsCombined_length_AND_15_CONC_ETC___d134,
	       IF_NOT_wrap64_setBoundsCombined_length_BIT_31__ETC___d207,
	       repBound__h4879,
	       IF_NOT_wrap64_setBoundsCombined_length_BIT_31__ETC___d216,
	       IF_NOT_wrap64_setBoundsCombined_length_BIT_31__ETC___d217,
	       IF_wrap64_setBoundsCombined_length_AND_15_CONC_ETC___d219,
	       IF_IF_NOT_wrap64_setBoundsCombined_length_BIT__ETC___d229,
	       wrap64_setBoundsCombined_cap_BITS_89_TO_58_8_A_ETC___d234 &&
	       _0b0_CONCAT_wrap64_setBoundsCombined_cap_BITS_8_ETC___d235,
	       x__h4954[31:0],
	       x__h5021[31:0] } ;

  // remaining internal signals
  assign IF_IF_NOT_wrap64_setBoundsCombined_length_BIT__ETC___d229 =
	     { (IF_NOT_wrap64_setBoundsCombined_length_BIT_31__ETC___d216 ==
		IF_wrap64_setBoundsCombined_length_AND_15_CONC_ETC___d219) ?
		 2'd0 :
		 ((IF_NOT_wrap64_setBoundsCombined_length_BIT_31__ETC___d216 &&
		   !IF_wrap64_setBoundsCombined_length_AND_15_CONC_ETC___d219) ?
		    2'd1 :
		    2'd3),
	       (IF_NOT_wrap64_setBoundsCombined_length_BIT_31__ETC___d217 ==
		IF_wrap64_setBoundsCombined_length_AND_15_CONC_ETC___d219) ?
		 2'd0 :
		 ((IF_NOT_wrap64_setBoundsCombined_length_BIT_31__ETC___d217 &&
		   !IF_wrap64_setBoundsCombined_length_AND_15_CONC_ETC___d219) ?
		    2'd1 :
		    2'd3) } ;
  assign IF_NOT_wrap64_setBoundsCombined_length_BIT_31__ETC___d207 =
	     (!wrap64_setBoundsCombined_length[31] &&
	      !wrap64_setBoundsCombined_length[30] &&
	      !wrap64_setBoundsCombined_length[29] &&
	      !wrap64_setBoundsCombined_length[28] &&
	      !wrap64_setBoundsCombined_length[27] &&
	      !wrap64_setBoundsCombined_length[26] &&
	      !wrap64_setBoundsCombined_length[25] &&
	      !wrap64_setBoundsCombined_length[24] &&
	      !wrap64_setBoundsCombined_length[23] &&
	      !wrap64_setBoundsCombined_length[22] &&
	      !wrap64_setBoundsCombined_length[21] &&
	      !wrap64_setBoundsCombined_length[20] &&
	      !wrap64_setBoundsCombined_length[19] &&
	      !wrap64_setBoundsCombined_length[18] &&
	      !wrap64_setBoundsCombined_length[17] &&
	      !wrap64_setBoundsCombined_length[16] &&
	      !wrap64_setBoundsCombined_length[15] &&
	      !wrap64_setBoundsCombined_length[14] &&
	      !wrap64_setBoundsCombined_length[13] &&
	      !wrap64_setBoundsCombined_length[12] &&
	      !wrap64_setBoundsCombined_length[11] &&
	      !wrap64_setBoundsCombined_length[10] &&
	      !wrap64_setBoundsCombined_length[9] &&
	      !wrap64_setBoundsCombined_length[8] &&
	      !wrap64_setBoundsCombined_length[7] &&
	      !wrap64_setBoundsCombined_length[6]) ?
	       { ret_bounds_topBits__h4842, x__h4632[7:0] } :
	       { ret_bounds_topBits__h4617[7:3],
		 3'd0,
		 ret_bounds_baseBits__h4793 } ;
  assign IF_NOT_wrap64_setBoundsCombined_length_BIT_31__ETC___d216 =
	     x__h4889[7:5] < repBound__h4879 ;
  assign IF_NOT_wrap64_setBoundsCombined_length_BIT_31__ETC___d217 =
	     x__h4886[7:5] < repBound__h4879 ;
  assign IF_wrap64_setBoundsCombined_length_AND_15_CONC_ETC___d134 =
	     (wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d48 &&
	      (wrap64_setBoundsCombined_length[31] ||
	       wrap64_setBoundsCombined_length[30] ||
	       wrap64_setBoundsCombined_length[29] ||
	       wrap64_setBoundsCombined_length[28] ||
	       wrap64_setBoundsCombined_length[27] ||
	       wrap64_setBoundsCombined_length[26] ||
	       wrap64_setBoundsCombined_length[25] ||
	       wrap64_setBoundsCombined_length[24] ||
	       wrap64_setBoundsCombined_length[23] ||
	       wrap64_setBoundsCombined_length[22] ||
	       wrap64_setBoundsCombined_length[21] ||
	       wrap64_setBoundsCombined_length[20] ||
	       wrap64_setBoundsCombined_length[19] ||
	       wrap64_setBoundsCombined_length[18] ||
	       wrap64_setBoundsCombined_length[17] ||
	       wrap64_setBoundsCombined_length[16] ||
	       wrap64_setBoundsCombined_length[15] ||
	       wrap64_setBoundsCombined_length[14] ||
	       wrap64_setBoundsCombined_length[13] ||
	       wrap64_setBoundsCombined_length[12] ||
	       wrap64_setBoundsCombined_length[11] ||
	       wrap64_setBoundsCombined_length[10] ||
	       wrap64_setBoundsCombined_length[9] ||
	       wrap64_setBoundsCombined_length[8] ||
	       wrap64_setBoundsCombined_length[7] ||
	       wrap64_setBoundsCombined_length[6])) ?
	       _25_MINUS_0_CONCAT_IF_wrap64_setBoundsCombined__ETC___d127 +
	       6'd1 :
	       _25_MINUS_0_CONCAT_IF_wrap64_setBoundsCombined__ETC___d127 ;
  assign IF_wrap64_setBoundsCombined_length_AND_15_CONC_ETC___d219 =
	     result_cap_addrBits__h4278[7:5] < repBound__h4879 ;
  assign NOT_0_CONCAT_wrap64_setBoundsCombined_length_O_ETC___d37 =
	     (mwLsbMask__h148 & top__h135) != (x__h4503 ^ y__h4504) ;
  assign NOT_0b0_CONCAT_wrap64_setBoundsCombined_cap_BI_ETC___d186 =
	     (top__h135 & lmaskLor__h139) != 34'd0 &&
	     (wrap64_setBoundsCombined_length[31] ||
	      wrap64_setBoundsCombined_length[30] ||
	      wrap64_setBoundsCombined_length[29] ||
	      wrap64_setBoundsCombined_length[28] ||
	      wrap64_setBoundsCombined_length[27] ||
	      wrap64_setBoundsCombined_length[26] ||
	      wrap64_setBoundsCombined_length[25] ||
	      wrap64_setBoundsCombined_length[24] ||
	      wrap64_setBoundsCombined_length[23] ||
	      wrap64_setBoundsCombined_length[22] ||
	      wrap64_setBoundsCombined_length[21] ||
	      wrap64_setBoundsCombined_length[20] ||
	      wrap64_setBoundsCombined_length[19] ||
	      wrap64_setBoundsCombined_length[18] ||
	      wrap64_setBoundsCombined_length[17] ||
	      wrap64_setBoundsCombined_length[16] ||
	      wrap64_setBoundsCombined_length[15] ||
	      wrap64_setBoundsCombined_length[14] ||
	      wrap64_setBoundsCombined_length[13] ||
	      wrap64_setBoundsCombined_length[12] ||
	      wrap64_setBoundsCombined_length[11] ||
	      wrap64_setBoundsCombined_length[10] ||
	      wrap64_setBoundsCombined_length[9] ||
	      wrap64_setBoundsCombined_length[8] ||
	      wrap64_setBoundsCombined_length[7] ||
	      wrap64_setBoundsCombined_length[6]) ;
  assign NOT_0b0_CONCAT_wrap64_setBoundsCombined_cap_BI_ETC___d197 =
	     (top__h135 & lmaskLo__h140) != 34'd0 &&
	     (wrap64_setBoundsCombined_length[31] ||
	      wrap64_setBoundsCombined_length[30] ||
	      wrap64_setBoundsCombined_length[29] ||
	      wrap64_setBoundsCombined_length[28] ||
	      wrap64_setBoundsCombined_length[27] ||
	      wrap64_setBoundsCombined_length[26] ||
	      wrap64_setBoundsCombined_length[25] ||
	      wrap64_setBoundsCombined_length[24] ||
	      wrap64_setBoundsCombined_length[23] ||
	      wrap64_setBoundsCombined_length[22] ||
	      wrap64_setBoundsCombined_length[21] ||
	      wrap64_setBoundsCombined_length[20] ||
	      wrap64_setBoundsCombined_length[19] ||
	      wrap64_setBoundsCombined_length[18] ||
	      wrap64_setBoundsCombined_length[17] ||
	      wrap64_setBoundsCombined_length[16] ||
	      wrap64_setBoundsCombined_length[15] ||
	      wrap64_setBoundsCombined_length[14] ||
	      wrap64_setBoundsCombined_length[13] ||
	      wrap64_setBoundsCombined_length[12] ||
	      wrap64_setBoundsCombined_length[11] ||
	      wrap64_setBoundsCombined_length[10] ||
	      wrap64_setBoundsCombined_length[9] ||
	      wrap64_setBoundsCombined_length[8] ||
	      wrap64_setBoundsCombined_length[7] ||
	      wrap64_setBoundsCombined_length[6]) ;
  assign _0b0_CONCAT_wrap64_setBoundsCombined_cap_BITS_8_ETC___d235 =
	     (top__h135 & lmaskLor__h139) == 34'd0 ||
	     !wrap64_setBoundsCombined_length[31] &&
	     !wrap64_setBoundsCombined_length[30] &&
	     !wrap64_setBoundsCombined_length[29] &&
	     !wrap64_setBoundsCombined_length[28] &&
	     !wrap64_setBoundsCombined_length[27] &&
	     !wrap64_setBoundsCombined_length[26] &&
	     !wrap64_setBoundsCombined_length[25] &&
	     !wrap64_setBoundsCombined_length[24] &&
	     !wrap64_setBoundsCombined_length[23] &&
	     !wrap64_setBoundsCombined_length[22] &&
	     !wrap64_setBoundsCombined_length[21] &&
	     !wrap64_setBoundsCombined_length[20] &&
	     !wrap64_setBoundsCombined_length[19] &&
	     !wrap64_setBoundsCombined_length[18] &&
	     !wrap64_setBoundsCombined_length[17] &&
	     !wrap64_setBoundsCombined_length[16] &&
	     !wrap64_setBoundsCombined_length[15] &&
	     !wrap64_setBoundsCombined_length[14] &&
	     !wrap64_setBoundsCombined_length[13] &&
	     !wrap64_setBoundsCombined_length[12] &&
	     !wrap64_setBoundsCombined_length[11] &&
	     !wrap64_setBoundsCombined_length[10] &&
	     !wrap64_setBoundsCombined_length[9] &&
	     !wrap64_setBoundsCombined_length[8] &&
	     !wrap64_setBoundsCombined_length[7] &&
	     !wrap64_setBoundsCombined_length[6] ;
  assign _25_MINUS_0_CONCAT_IF_wrap64_setBoundsCombined__ETC___d127 =
	     6'd25 -
	     { 1'd0,
	       wrap64_setBoundsCombined_length[31] ?
		 5'd0 :
		 (wrap64_setBoundsCombined_length[30] ?
		    5'd1 :
		    (wrap64_setBoundsCombined_length[29] ?
		       5'd2 :
		       (wrap64_setBoundsCombined_length[28] ?
			  5'd3 :
			  (wrap64_setBoundsCombined_length[27] ?
			     5'd4 :
			     (wrap64_setBoundsCombined_length[26] ?
				5'd5 :
				(wrap64_setBoundsCombined_length[25] ?
				   5'd6 :
				   (wrap64_setBoundsCombined_length[24] ?
				      5'd7 :
				      (wrap64_setBoundsCombined_length[23] ?
					 5'd8 :
					 (wrap64_setBoundsCombined_length[22] ?
					    5'd9 :
					    (wrap64_setBoundsCombined_length[21] ?
					       5'd10 :
					       (wrap64_setBoundsCombined_length[20] ?
						  5'd11 :
						  (wrap64_setBoundsCombined_length[19] ?
						     5'd12 :
						     (wrap64_setBoundsCombined_length[18] ?
							5'd13 :
							(wrap64_setBoundsCombined_length[17] ?
							   5'd14 :
							   (wrap64_setBoundsCombined_length[16] ?
							      5'd15 :
							      (wrap64_setBoundsCombined_length[15] ?
								 5'd16 :
								 (wrap64_setBoundsCombined_length[14] ?
								    5'd17 :
								    (wrap64_setBoundsCombined_length[13] ?
								       5'd18 :
								       (wrap64_setBoundsCombined_length[12] ?
									  5'd19 :
									  (wrap64_setBoundsCombined_length[11] ?
									     5'd20 :
									     (wrap64_setBoundsCombined_length[10] ?
										5'd21 :
										(wrap64_setBoundsCombined_length[9] ?
										   5'd22 :
										   (wrap64_setBoundsCombined_length[8] ?
										      5'd23 :
										      (wrap64_setBoundsCombined_length[7] ?
											 5'd24 :
											 5'd25)))))))))))))))))))))))) } ;
  assign _theResult_____1_fst__h4965 =
	     ((wrap64_setBoundsCombined_length &
	       { 4'd0,
		 wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d16[31:4] }) !=
	      32'd0 &&
	      (wrap64_setBoundsCombined_length[31] ||
	       wrap64_setBoundsCombined_length[30] ||
	       wrap64_setBoundsCombined_length[29] ||
	       wrap64_setBoundsCombined_length[28] ||
	       wrap64_setBoundsCombined_length[27] ||
	       wrap64_setBoundsCombined_length[26] ||
	       wrap64_setBoundsCombined_length[25] ||
	       wrap64_setBoundsCombined_length[24] ||
	       wrap64_setBoundsCombined_length[23] ||
	       wrap64_setBoundsCombined_length[22] ||
	       wrap64_setBoundsCombined_length[21] ||
	       wrap64_setBoundsCombined_length[20] ||
	       wrap64_setBoundsCombined_length[19] ||
	       wrap64_setBoundsCombined_length[18] ||
	       wrap64_setBoundsCombined_length[17] ||
	       wrap64_setBoundsCombined_length[16] ||
	       wrap64_setBoundsCombined_length[15] ||
	       wrap64_setBoundsCombined_length[14] ||
	       wrap64_setBoundsCombined_length[13] ||
	       wrap64_setBoundsCombined_length[12] ||
	       wrap64_setBoundsCombined_length[11] ||
	       wrap64_setBoundsCombined_length[10] ||
	       wrap64_setBoundsCombined_length[9] ||
	       wrap64_setBoundsCombined_length[8] ||
	       wrap64_setBoundsCombined_length[7] ||
	       wrap64_setBoundsCombined_length[6])) ?
	       newLengthRounded__h4963 :
	       newLength___2__h4962 ;
  assign _theResult_____3_fst_bounds_topBits__h4621 =
	     NOT_0b0_CONCAT_wrap64_setBoundsCombined_cap_BI_ETC___d197 ?
	       x__h4811[8:1] + 8'b00001000 :
	       x__h4811[8:1] ;
  assign baseMask___1__h4964 =
	     (wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d45 &&
	      NOT_0b0_CONCAT_wrap64_setBoundsCombined_cap_BI_ETC___d186) ?
	       { 5'd31,
		 ~wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d16[31:3] } :
	       y__h4982 ;
  assign base__h132 = { 2'b0, wrap64_setBoundsCombined_cap[89:58] } ;
  assign lmaskLo__h140 =
	     { 5'd0,
	       wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d16[31:3] } ;
  assign lmaskLor__h139 =
	     { 6'd0,
	       wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d16[31:4] } ;
  assign mwLsbMask__h148 = lmaskLor__h139 ^ lmaskLo__h140 ;
  assign newLengthRounded__h4961 = newLength__h158 + oneInLsb__h4960 ;
  assign newLengthRounded__h4963 = newLengthRounded__h4961 & y__h4982 ;
  assign newLength___2__h4962 =
	     { 2'd0,
	       wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d20 } ;
  assign newLength__h158 = { 2'b0, wrap64_setBoundsCombined_length } ;
  assign oneInLsb__h4960 =
	     { 5'd0,
	       wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC__q1[31:3] } ;
  assign repBound__h4879 = x__h4886[7:5] - 3'b001 ;
  assign result_cap_addrBits__h4278 =
	     (wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d48 &&
	      (wrap64_setBoundsCombined_length[31] ||
	       wrap64_setBoundsCombined_length[30] ||
	       wrap64_setBoundsCombined_length[29] ||
	       wrap64_setBoundsCombined_length[28] ||
	       wrap64_setBoundsCombined_length[27] ||
	       wrap64_setBoundsCombined_length[26] ||
	       wrap64_setBoundsCombined_length[25] ||
	       wrap64_setBoundsCombined_length[24] ||
	       wrap64_setBoundsCombined_length[23] ||
	       wrap64_setBoundsCombined_length[22] ||
	       wrap64_setBoundsCombined_length[21] ||
	       wrap64_setBoundsCombined_length[20] ||
	       wrap64_setBoundsCombined_length[19] ||
	       wrap64_setBoundsCombined_length[18] ||
	       wrap64_setBoundsCombined_length[17] ||
	       wrap64_setBoundsCombined_length[16] ||
	       wrap64_setBoundsCombined_length[15] ||
	       wrap64_setBoundsCombined_length[14] ||
	       wrap64_setBoundsCombined_length[13] ||
	       wrap64_setBoundsCombined_length[12] ||
	       wrap64_setBoundsCombined_length[11] ||
	       wrap64_setBoundsCombined_length[10] ||
	       wrap64_setBoundsCombined_length[9] ||
	       wrap64_setBoundsCombined_length[8] ||
	       wrap64_setBoundsCombined_length[7] ||
	       wrap64_setBoundsCombined_length[6])) ?
	       x__h4632[8:1] :
	       x__h4632[7:0] ;
  assign ret_bounds_baseBits__h4793 =
	     { result_cap_addrBits__h4278[7:3], 3'd0 } ;
  assign ret_bounds_topBits__h4617 =
	     (wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d48 &&
	      (wrap64_setBoundsCombined_length[31] ||
	       wrap64_setBoundsCombined_length[30] ||
	       wrap64_setBoundsCombined_length[29] ||
	       wrap64_setBoundsCombined_length[28] ||
	       wrap64_setBoundsCombined_length[27] ||
	       wrap64_setBoundsCombined_length[26] ||
	       wrap64_setBoundsCombined_length[25] ||
	       wrap64_setBoundsCombined_length[24] ||
	       wrap64_setBoundsCombined_length[23] ||
	       wrap64_setBoundsCombined_length[22] ||
	       wrap64_setBoundsCombined_length[21] ||
	       wrap64_setBoundsCombined_length[20] ||
	       wrap64_setBoundsCombined_length[19] ||
	       wrap64_setBoundsCombined_length[18] ||
	       wrap64_setBoundsCombined_length[17] ||
	       wrap64_setBoundsCombined_length[16] ||
	       wrap64_setBoundsCombined_length[15] ||
	       wrap64_setBoundsCombined_length[14] ||
	       wrap64_setBoundsCombined_length[13] ||
	       wrap64_setBoundsCombined_length[12] ||
	       wrap64_setBoundsCombined_length[11] ||
	       wrap64_setBoundsCombined_length[10] ||
	       wrap64_setBoundsCombined_length[9] ||
	       wrap64_setBoundsCombined_length[8] ||
	       wrap64_setBoundsCombined_length[7] ||
	       wrap64_setBoundsCombined_length[6])) ?
	       _theResult_____3_fst_bounds_topBits__h4621 :
	       ret_bounds_topBits__h4842 ;
  assign ret_bounds_topBits__h4842 =
	     NOT_0b0_CONCAT_wrap64_setBoundsCombined_cap_BI_ETC___d186 ?
	       x__h4850[7:0] :
	       x__h4811[7:0] ;
  assign top__h135 = base__h132 + newLength__h158 ;
  assign wrap64_setBoundsCombined_cap_BITS_89_TO_58_8_A_ETC___d234 =
	     (wrap64_setBoundsCombined_cap[89:58] &
	      { 4'd0,
		wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d16[31:4] }) ==
	     32'd0 ||
	     !wrap64_setBoundsCombined_length[31] &&
	     !wrap64_setBoundsCombined_length[30] &&
	     !wrap64_setBoundsCombined_length[29] &&
	     !wrap64_setBoundsCombined_length[28] &&
	     !wrap64_setBoundsCombined_length[27] &&
	     !wrap64_setBoundsCombined_length[26] &&
	     !wrap64_setBoundsCombined_length[25] &&
	     !wrap64_setBoundsCombined_length[24] &&
	     !wrap64_setBoundsCombined_length[23] &&
	     !wrap64_setBoundsCombined_length[22] &&
	     !wrap64_setBoundsCombined_length[21] &&
	     !wrap64_setBoundsCombined_length[20] &&
	     !wrap64_setBoundsCombined_length[19] &&
	     !wrap64_setBoundsCombined_length[18] &&
	     !wrap64_setBoundsCombined_length[17] &&
	     !wrap64_setBoundsCombined_length[16] &&
	     !wrap64_setBoundsCombined_length[15] &&
	     !wrap64_setBoundsCombined_length[14] &&
	     !wrap64_setBoundsCombined_length[13] &&
	     !wrap64_setBoundsCombined_length[12] &&
	     !wrap64_setBoundsCombined_length[11] &&
	     !wrap64_setBoundsCombined_length[10] &&
	     !wrap64_setBoundsCombined_length[9] &&
	     !wrap64_setBoundsCombined_length[8] &&
	     !wrap64_setBoundsCombined_length[7] &&
	     !wrap64_setBoundsCombined_length[6] ;
  assign wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d20 =
	     wrap64_setBoundsCombined_length &
	     { 4'd15,
	       ~wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d16[31:4] } ;
  assign wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d38 =
	     wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d20 ==
	     (wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d16 ^
	      { 3'd0,
		wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d16[31:3] }) &&
	     NOT_0_CONCAT_wrap64_setBoundsCombined_length_O_ETC___d37 ;
  assign wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d45 =
	     wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d20 ==
	     (wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d16 ^
	      { 4'd0,
		wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d16[31:4] }) ;
  assign wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d47 =
	     wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d45 &&
	     (NOT_0_CONCAT_wrap64_setBoundsCombined_length_O_ETC___d37 ||
	      (top__h135 & lmaskLor__h139) != 34'd0) ;
  assign wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d48 =
	     wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d38 &&
	     (top__h135 & lmaskLor__h139) != 34'd0 ||
	     wrap64_setBoundsCombined_length_AND_15_CONCAT__ETC___d47 ;
  assign wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d10 =
	     wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d7 |
	     { 4'd0,
	       wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d7[31:4] } ;
  assign wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d13 =
	     wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d10 |
	     { 8'd0,
	       wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d10[31:8] } ;
  assign wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d16 =
	     wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d13 |
	     { 16'd0,
	       wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d13[31:16] } ;
  assign wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d4 =
	     wrap64_setBoundsCombined_length |
	     { 1'd0, wrap64_setBoundsCombined_length[31:1] } ;
  assign wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d7 =
	     wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d4 |
	     { 2'd0,
	       wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d4[31:2] } ;
  assign wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC__q1 =
	     wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d16 ^
	     { 1'd0,
	       wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d16[31:1] } ;
  assign x__h4503 = mwLsbMask__h148 & base__h132 ;
  assign x__h4632 =
	     base__h132 >>
	     _25_MINUS_0_CONCAT_IF_wrap64_setBoundsCombined__ETC___d127 ;
  assign x__h4811 =
	     top__h135 >>
	     _25_MINUS_0_CONCAT_IF_wrap64_setBoundsCombined__ETC___d127 ;
  assign x__h4850 = x__h4811[8:0] + 9'b000001000 ;
  assign x__h4886 =
	     (!wrap64_setBoundsCombined_length[31] &&
	      !wrap64_setBoundsCombined_length[30] &&
	      !wrap64_setBoundsCombined_length[29] &&
	      !wrap64_setBoundsCombined_length[28] &&
	      !wrap64_setBoundsCombined_length[27] &&
	      !wrap64_setBoundsCombined_length[26] &&
	      !wrap64_setBoundsCombined_length[25] &&
	      !wrap64_setBoundsCombined_length[24] &&
	      !wrap64_setBoundsCombined_length[23] &&
	      !wrap64_setBoundsCombined_length[22] &&
	      !wrap64_setBoundsCombined_length[21] &&
	      !wrap64_setBoundsCombined_length[20] &&
	      !wrap64_setBoundsCombined_length[19] &&
	      !wrap64_setBoundsCombined_length[18] &&
	      !wrap64_setBoundsCombined_length[17] &&
	      !wrap64_setBoundsCombined_length[16] &&
	      !wrap64_setBoundsCombined_length[15] &&
	      !wrap64_setBoundsCombined_length[14] &&
	      !wrap64_setBoundsCombined_length[13] &&
	      !wrap64_setBoundsCombined_length[12] &&
	      !wrap64_setBoundsCombined_length[11] &&
	      !wrap64_setBoundsCombined_length[10] &&
	      !wrap64_setBoundsCombined_length[9] &&
	      !wrap64_setBoundsCombined_length[8] &&
	      !wrap64_setBoundsCombined_length[7] &&
	      !wrap64_setBoundsCombined_length[6]) ?
	       result_cap_addrBits__h4278 :
	       ret_bounds_baseBits__h4793 ;
  assign x__h4889 =
	     (!wrap64_setBoundsCombined_length[31] &&
	      !wrap64_setBoundsCombined_length[30] &&
	      !wrap64_setBoundsCombined_length[29] &&
	      !wrap64_setBoundsCombined_length[28] &&
	      !wrap64_setBoundsCombined_length[27] &&
	      !wrap64_setBoundsCombined_length[26] &&
	      !wrap64_setBoundsCombined_length[25] &&
	      !wrap64_setBoundsCombined_length[24] &&
	      !wrap64_setBoundsCombined_length[23] &&
	      !wrap64_setBoundsCombined_length[22] &&
	      !wrap64_setBoundsCombined_length[21] &&
	      !wrap64_setBoundsCombined_length[20] &&
	      !wrap64_setBoundsCombined_length[19] &&
	      !wrap64_setBoundsCombined_length[18] &&
	      !wrap64_setBoundsCombined_length[17] &&
	      !wrap64_setBoundsCombined_length[16] &&
	      !wrap64_setBoundsCombined_length[15] &&
	      !wrap64_setBoundsCombined_length[14] &&
	      !wrap64_setBoundsCombined_length[13] &&
	      !wrap64_setBoundsCombined_length[12] &&
	      !wrap64_setBoundsCombined_length[11] &&
	      !wrap64_setBoundsCombined_length[10] &&
	      !wrap64_setBoundsCombined_length[9] &&
	      !wrap64_setBoundsCombined_length[8] &&
	      !wrap64_setBoundsCombined_length[7] &&
	      !wrap64_setBoundsCombined_length[6]) ?
	       ret_bounds_topBits__h4617 :
	       { ret_bounds_topBits__h4617[7:3], 3'd0 } ;
  assign x__h4954 =
	     (wrap64_setBoundsCombined_length[31] ||
	      wrap64_setBoundsCombined_length[30] ||
	      wrap64_setBoundsCombined_length[29] ||
	      wrap64_setBoundsCombined_length[28] ||
	      wrap64_setBoundsCombined_length[27] ||
	      wrap64_setBoundsCombined_length[26] ||
	      wrap64_setBoundsCombined_length[25] ||
	      wrap64_setBoundsCombined_length[24] ||
	      wrap64_setBoundsCombined_length[23] ||
	      wrap64_setBoundsCombined_length[22] ||
	      wrap64_setBoundsCombined_length[21] ||
	      wrap64_setBoundsCombined_length[20] ||
	      wrap64_setBoundsCombined_length[19] ||
	      wrap64_setBoundsCombined_length[18] ||
	      wrap64_setBoundsCombined_length[17] ||
	      wrap64_setBoundsCombined_length[16] ||
	      wrap64_setBoundsCombined_length[15] ||
	      wrap64_setBoundsCombined_length[14] ||
	      wrap64_setBoundsCombined_length[13] ||
	      wrap64_setBoundsCombined_length[12] ||
	      wrap64_setBoundsCombined_length[11] ||
	      wrap64_setBoundsCombined_length[10] ||
	      wrap64_setBoundsCombined_length[9] ||
	      wrap64_setBoundsCombined_length[8] ||
	      wrap64_setBoundsCombined_length[7] ||
	      wrap64_setBoundsCombined_length[6]) ?
	       _theResult_____1_fst__h4965 :
	       newLength__h158 ;
  assign x__h5021 =
	     (wrap64_setBoundsCombined_length[31] ||
	      wrap64_setBoundsCombined_length[30] ||
	      wrap64_setBoundsCombined_length[29] ||
	      wrap64_setBoundsCombined_length[28] ||
	      wrap64_setBoundsCombined_length[27] ||
	      wrap64_setBoundsCombined_length[26] ||
	      wrap64_setBoundsCombined_length[25] ||
	      wrap64_setBoundsCombined_length[24] ||
	      wrap64_setBoundsCombined_length[23] ||
	      wrap64_setBoundsCombined_length[22] ||
	      wrap64_setBoundsCombined_length[21] ||
	      wrap64_setBoundsCombined_length[20] ||
	      wrap64_setBoundsCombined_length[19] ||
	      wrap64_setBoundsCombined_length[18] ||
	      wrap64_setBoundsCombined_length[17] ||
	      wrap64_setBoundsCombined_length[16] ||
	      wrap64_setBoundsCombined_length[15] ||
	      wrap64_setBoundsCombined_length[14] ||
	      wrap64_setBoundsCombined_length[13] ||
	      wrap64_setBoundsCombined_length[12] ||
	      wrap64_setBoundsCombined_length[11] ||
	      wrap64_setBoundsCombined_length[10] ||
	      wrap64_setBoundsCombined_length[9] ||
	      wrap64_setBoundsCombined_length[8] ||
	      wrap64_setBoundsCombined_length[7] ||
	      wrap64_setBoundsCombined_length[6]) ?
	       baseMask___1__h4964 :
	       34'h3FFFFFFFF ;
  assign y__h4504 = mwLsbMask__h148 & newLength__h158 ;
  assign y__h4982 =
	     { 6'd63,
	       ~wrap64_setBoundsCombined_length_OR_0_CONCAT_wr_ETC___d16[31:4] } ;
endmodule  // module_wrap64_setBoundsCombined

