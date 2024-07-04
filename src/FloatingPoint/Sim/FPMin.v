import "DPI-C" function int c_FPMin(input int a, input int b);

// Latency: 1 cycle
module FPMin (
		input  wire        clk,
		input  wire        areset,
		input  wire [31:0] a,
		input  wire [31:0] b,
		output wire [31:0] q
	);

  parameter LATENCY = 1;
  reg [31:0] result[LATENCY-1:0];

  generate
    genvar i;
    for (i = 0; i < LATENCY-1; i=i+1) begin
      always @(posedge clk) begin
        result[i] <= result[i+1];
      end
    end
  endgenerate

  always @(posedge clk) begin
    result[LATENCY-1] <= c_FPMin(a, b);
  end

  assign q = result[0];

endmodule
