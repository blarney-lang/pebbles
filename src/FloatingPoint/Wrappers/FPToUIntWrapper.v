module FPToUIntWrapper (
    input  wire        clock,
    input  wire        reset,
    input  wire [31:0] a,
    output wire [31:0] q
  );

  FPToUInt FPToUIntInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .q(q)
  );
endmodule
