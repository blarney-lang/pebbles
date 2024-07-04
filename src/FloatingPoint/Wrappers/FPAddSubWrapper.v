module FPAddSubWrapper (
    input  wire        clock,
    input  wire        reset,
    input  wire [31:0] a,
    input  wire [31:0] b,
    output wire [31:0] q,
    input  wire [0:0]  opSel
  );

  parameter LATENCY = 3;

`ifdef _SIM_
  FPAddSub#(.LATENCY(LATENCY)) FPAddSubInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .b(b)
  , .q(q)
  , .opSel(opSel)
  );
`else
  FPAddSub FPAddSubInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .b(b)
  , .q(q)
  , .opSel(opSel)
  );
`endif
endmodule
