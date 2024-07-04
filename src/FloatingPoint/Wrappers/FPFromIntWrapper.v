module FPFromIntWrapper (
    input  clock
    input  reset,
    input  wire [31:0] a,
    output wire [31:0] q
  );

  parameter LATENCY = 7;

  FPFromInt#(.LATENCY(LATENCY)) FPFromIntInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .q(q)
  )
endmodule
