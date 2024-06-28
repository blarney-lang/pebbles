module FPToInt33Wrapper (
    input  wire        clock,
    input  wire        reset,
    input  wire [31:0] a,
    output wire [32:0] q
  );

  FPToInt33 FPToInt33Inst (
    .clk(clock)
  , .areset(reset)
  , .a(a)
  , .q(q)
  );
endmodule
