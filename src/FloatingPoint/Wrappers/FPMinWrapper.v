module FPMinWrapper (
    input  clock,
    input  reset,
    input  wire [31:0] a,
    input  wire [31:0] b,
    output wire [31:0] q
  );

  parameter LATENCY = 1;

`ifdef _SIM_
  FPMin#(.LATENCY(LATENCY)) FPMinInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .b(b)
  , .q(q)
  );
`else
  FPMin FPMinInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .b(b)
  , .q(q)
  );
`endif
endmodule
