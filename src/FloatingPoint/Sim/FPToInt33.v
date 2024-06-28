import "DPI-C" function longint c_FPToInt64(input int a);

// Latency: 3 cycles
module FPToInt33 (
		input  wire        clk,    //    clk.clk
		input  wire        areset, // areset.reset
		input  wire [31:0] a,      //      a.a
		output wire [32:0] q       //      q.q
	);

  parameter LATENCY = 3;
  reg [32:0] result[LATENCY-1:0];

  generate
    genvar i;
    for (i = 0; i < LATENCY-1; i=i+1) begin
      always @(posedge clk) begin
        result[i] <= result[i+1];
      end
    end
  endgenerate

  always @(posedge clk) begin
    result[LATENCY-1] <= (c_FPToInt64(a))[32:0];
  end

  assign q = result[0];

endmodule
