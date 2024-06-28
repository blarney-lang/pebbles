module FPCompareLTEWrapper (
    input  wire        clock,
    input  wire        reset,
    input  wire [31:0] a,
    input  wire [31:0] b,
    output wire [0:0]  q
  );

  FPCompareLTE FPCompareLTEInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .b(b)
  , .q(q)
  );
endmodule
