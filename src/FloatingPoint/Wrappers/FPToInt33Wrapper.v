module FPToInt33Wrapper (
    input  wire        clock,
    input  wire        reset,
    input  wire [31:0] a,
    output wire [32:0] q
  );

  parameter LATENCY = 3;

  FPToInt33#(.LATENCY(LATENCY)) FPToInt33Inst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .q(q)
  );
endmodule
