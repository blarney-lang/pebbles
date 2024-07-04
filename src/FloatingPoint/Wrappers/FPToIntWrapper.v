module FPToIntWrapper (
    input  wire        clock,
    input  wire        reset,
    input  wire [31:0] a,
    output wire [31:0] q
  );

  parameter LATENCY = 3;

`ifdef _SIM_
  FPToInt#(.LATENCY(LATENCY)) FPToIntInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .q(q)
  );
`else
  FPToInt FPToIntInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .q(q)
  );
`endif
endmodule
