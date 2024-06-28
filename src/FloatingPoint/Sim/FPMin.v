import "DPI-C" function int c_FPMin(input int a, input int b);

// Latency: 1 cycle
module FPMin (
		input  wire        clk,
		input  wire        areset,
		input  wire [31:0] a,
		input  wire [31:0] b,
		output wire [31:0] q
	);

  reg [31:0] result;

  always @(posedge clk) begin
    result <= c_FPMin(a, b);
  end

  assign q = result;
endmodule
