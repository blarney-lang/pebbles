//
// Generated by Bluespec Compiler (build 14ff62d)
//
// On Mon Jul 25 15:00:14 BST 2022
//
//
// Ports:
// Name                         I/O  size props
// wrap64_almightyCapPipe         O    91 const
//
// No combinational paths from inputs to outputs
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

module module_wrap64_almightyCapPipe(wrap64_almightyCapPipe);
  // value method wrap64_almightyCapPipe
  output [90 : 0] wrap64_almightyCapPipe;

  // signals for module outputs
  wire [90 : 0] wrap64_almightyCapPipe;

  // value method wrap64_almightyCapPipe
  assign wrap64_almightyCapPipe = 91'h40000000003FFDF690003F0 ;
endmodule  // module_wrap64_almightyCapPipe

