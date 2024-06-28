module FPDivWrapper (
    input  wire clock,
    input  wire reset,
    input  wire [31:0] a,
    input  wire [31:0] b,
    output wire [31:0] q
  );

  FPDiv FPDivInst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .b(b)
  , .q(q)
  );
endmodule
