import "DPI-C" function int c_FPCompareEq(input int a, input int b);

// Latency: 1 cycle
module FPCompareEq (
		input  wire        clk,    //    clk.clk
		input  wire        areset, // areset.reset
		input  wire [31:0] a,      //      a.a
		input  wire [31:0] b,      //      b.b
		output wire [0:0]  q       //      q.q
	);

  reg [31:0] result;

  always @(posedge clk) begin
    result <= c_FPCompareEq(a, b);
  end

  assign q = result[0];
endmodule
