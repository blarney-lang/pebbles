module FPFromInt33Wrapper (
    input  clock,
    input  reset,
    input  wire [32:0] a,
    output wire [31:0] q
  );

  parameter LATENCY = 7;

  FPFromInt33#(.LATENCY(LATENCY)) FPFromInt33Inst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .q(q)
  );
endmodule
