module FPCompareLTEWrapper (
    input  wire        clock,
    input  wire        reset,
    input  wire [31:0] a,
    input  wire [31:0] b,
    output wire [0:0]  q
  );

  parameter LATENCY = 2;

`ifdef _SIM_
  FPCompareLTE#(.LATENCY(LATENCY)) FPCompareLTEInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .b(b)
  , .q(q)
  );
`else
  FPCompareLTE FPCompareLTEInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .b(b)
  , .q(q)
  );
`endif
endmodule
