module FPSqrt (
    input  clk,
    input  areset,
    input  wire [31:0] a,
    output wire [31:0] q
  );

  wire [33:0] fa;
  wire [33:0] fq;

  FFromIEEE FFromIEEEInst (
    .clk(clk),
    .X(a),
    .R(fa)
  );

  FFPSqrt FFPSqrtInst (
    .clk(clk),
    .X(fa),
    .R(fq)
  );

  FToIEEE FToIEEEInst (
    .clk(clk),
    .X(fq),
    .R(q)
  );

endmodule
