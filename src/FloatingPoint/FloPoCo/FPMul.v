module FPMul (
    input  clk,
    input  areset,
    input  wire [31:0] a,
    input  wire [31:0] b,
    output wire [31:0] q
  );

  wire [33:0] fa;
  wire [33:0] fb;
  wire [33:0] fq;

  FFromIEEE FFromIEEEInst1 (
    .clk(clk),
    .X(a),
    .R(fa)
  );

  FFromIEEE FFromIEEEInst2 (
    .clk(clk),
    .X(b),
    .R(fb)
  );

  FFPMult FFPMultInst (
    .clk(clk),
    .X(fa),
    .Y(fb),
    .R(fq)
  );

  FToIEEE FToIEEEInst (
    .clk(clk),
    .X(fq),
    .R(q)
  );

endmodule
