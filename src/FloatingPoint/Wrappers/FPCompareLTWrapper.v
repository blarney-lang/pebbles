module FPCompareLTWrapper (
    input  wire        clock,
    input  wire        reset,
    input  wire [31:0] a,
    input  wire [31:0] b,
    output wire [0:0]  q
  );

  parameter LATENCY = 2;

  FPCompareLT#(.LATENCY(LATENCY)) FPCompareLTInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .b(b)
  , .q(q)
  );
endmodule
