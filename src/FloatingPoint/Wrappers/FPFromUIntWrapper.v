module FPFromUIntWrapper (
    input  clock
    input  reset,
    input  wire [31:0] a,
    output wire [31:0] q
  );

  FPFromUInt FPFromUIntInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .q(q)
  )
endmodule
