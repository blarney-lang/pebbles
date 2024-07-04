module FPSqrtWrapper (
    input  clock,
    input  reset,
    input  wire [31:0] a,
    output wire [31:0] q
  );

  parameter LATENCY = 16;

`ifdef _SIM_
  FPSqrt#(.LATENCY(LATENCY)) FPSqrtInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .q(q)
  );
`else
  FPSqrt FPSqrtInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .q(q)
  );
`endif
endmodule
