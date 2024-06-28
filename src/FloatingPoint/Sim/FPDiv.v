import "DPI-C" function int c_FPDiv(input int a, input int b);

// Latency: 32 cycles
module FPDiv (
		input  wire        clk,    //    clk.clk
		input  wire        areset, // areset.reset
		input  wire [31:0] a,      //      a.a
		input  wire [31:0] b,      //      b.b
		output wire [31:0] q       //      q.q
	);

  parameter LATENCY = 32;
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
    result[LATENCY-1] <= c_FPDiv(a, b);   
  end

  assign q = result[0];

endmodule
