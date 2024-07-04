module FPToInt33Wrapper (
    input  wire        clock,
    input  wire        reset,
    input  wire [31:0] a,
    output wire [32:0] q
  );

  parameter LATENCY = 3;

`ifdef _SIM_
  FPToInt33#(.LATENCY(LATENCY)) FPToInt33Inst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .q(q)
  );
`else
  FPToInt33 FPToInt33Inst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .q(q)
  );
`endif
endmodule
