module FPMulWrapper (
    input  clock,
    input  reset,
    input  wire [31:0] a,
    input  wire [31:0] b,
    output wire [31:0] q
  );

  parameter LATENCY = 3;

`ifdef _SIM_
  FPMul#(.LATENCY(LATENCY)) FPMulInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .b(b)
  , .q(q)
  );
`else
  FPMul FPMulInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .b(b)
  , .q(q)
  );
`endif
endmodule
