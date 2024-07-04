module FPMinWrapper (
    input  clock,
    input  reset,
    input  wire [31:0] a,
    input  wire [31:0] b,
    output wire [31:0] q
  );

  parameter LATENCY = 1;

  FPMin#(.LATENCY(LATENCY)) FPMinInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .b(b)
  , .q(q)
  );
endmodule
