//
// Generated by Bluespec Compiler, version 2022.01-37-gaf852df5 (build af852df5)
//
// On Fri Jul  5 14:25:22 BST 2024
//
//
// Ports:
// Name                         I/O  size props
// wrap64_getKind                 O     7
// wrap64_getKind_cap             I    91
//
// Combinational paths from inputs to outputs:
//   wrap64_getKind_cap -> wrap64_getKind
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

module module_wrap64_getKind(wrap64_getKind_cap,
			     wrap64_getKind);
  // value method wrap64_getKind
  input  [90 : 0] wrap64_getKind_cap;
  output [6 : 0] wrap64_getKind;

  // signals for module outputs
  wire [6 : 0] wrap64_getKind;

  // remaining internal signals
  reg [2 : 0] CASE_wrap64_getKind_cap_BITS_36_TO_33_12_3_13__ETC__q1;

  // value method wrap64_getKind
  assign wrap64_getKind =
	     { CASE_wrap64_getKind_cap_BITS_36_TO_33_12_3_13__ETC__q1,
	       wrap64_getKind_cap[36:33] } ;

  // remaining internal signals
  always@(wrap64_getKind_cap)
  begin
    case (wrap64_getKind_cap[36:33])
      4'd12: CASE_wrap64_getKind_cap_BITS_36_TO_33_12_3_13__ETC__q1 = 3'd3;
      4'd13: CASE_wrap64_getKind_cap_BITS_36_TO_33_12_3_13__ETC__q1 = 3'd2;
      4'd14: CASE_wrap64_getKind_cap_BITS_36_TO_33_12_3_13__ETC__q1 = 3'd1;
      4'd15: CASE_wrap64_getKind_cap_BITS_36_TO_33_12_3_13__ETC__q1 = 3'd0;
      default: CASE_wrap64_getKind_cap_BITS_36_TO_33_12_3_13__ETC__q1 = 3'd4;
    endcase
  end
endmodule  // module_wrap64_getKind

