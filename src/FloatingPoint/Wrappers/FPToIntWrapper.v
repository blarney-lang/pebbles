module FPToIntWrapper (
    input  wire        clock,
    input  wire        reset,
    input  wire [31:0] a,
    output wire [31:0] q
  );

  parameter LATENCY = 3;

  FPToInt#(.LATENCY(LATENCY)) FPToIntInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .q(q)
  );
endmodule
