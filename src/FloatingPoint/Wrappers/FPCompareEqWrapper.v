module FPCompareEqWrapper (
    input  wire        clock,
    input  wire        reset,
    input  wire [31:0] a,
    input  wire [31:0] b,
    output wire [0:0]  q
  );

  parameter LATENCY = 1;

  FPCompareEq#(.LATENCY(LATENCY)) FPCompareEqInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .b(b)
  , .q(q)
  );
endmodule
